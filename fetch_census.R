library(httr2)
library(dplyr)
library(jsonlite)

# Free key: https://api.census.gov/data/key_signup.html
census_key <- Sys.getenv("CENSUS_API_KEY")

# One row per tab. To add a tracker, add a row: program = EITS abbreviation.
# CODES MARKED "verify" ARE BEST GUESSES. Check them with explore("program") before trusting them.
# scale: multiply raw values (e.g. 0.001 turns $ millions into $ billions)
series <- tribble(
  ~key, ~program, ~cat, ~dtype, ~adj, ~tab, ~title, ~prefix, ~suffix, ~scale, ~dec, ~desc,

  "retail", "marts", "44X72", "SM", "yes", "Retail Sales",
  "Retail & Food Services Sales (Monthly, Seasonally Adjusted)", "$", "B", 0.001, 1,
  "Estimated sales at U.S. retail stores and restaurants, in billions of dollars per month, from the Census Bureau's advance retail survey. Seasonally adjusted means regular seasonal swings, such as holiday shopping, are removed so months can be compared directly. The figures are not adjusted for inflation, so higher prices alone can push the line up, and early estimates are revised later.",

  "starts", "resconst", "ASTARTS", "TOTAL", "yes", "Housing Starts",
  "Housing Starts (Thousands, Annual Rate, Seasonally Adjusted)", "", "K", 1, 0,
  "The pace of new home construction, shown as an annual rate in thousands: the month's activity scaled up to a full year. A reading of 1,275K means builders are on track to start about 1.3 million homes over a year, not that many started in one month. Seasonally adjusted, covering privately owned housing units.",

  "newsales", "ressales", "ASOLD", "TOTAL", "yes", "New Home Sales",
  "New Home Sales (Thousands, Annual Rate, Seasonally Adjusted)", "", "K", 1, 0,
  "New single-family homes sold, shown as an annual rate in thousands, so 684K means a pace of about 684,000 sales over a year. Sales are counted when a contract is signed, not when the sale closes. The figures come from a sample and are often revised substantially, so single-month changes are noisy.",

  "vip", "vip", "AXXXX", "T", "yes", "Construction",
  "Construction Spending (Annual Rate, Seasonally Adjusted)", "$", "B", 0.001, 0,
  "The value of construction work on homes, commercial buildings, roads and other projects (private and public combined), in billions of dollars at a seasonally adjusted annual rate. $2,203B means a pace of about $2.2 trillion a year. Not adjusted for inflation, so rising building costs can lift spending without more being built.",

  "durable", "advm3", "MDM", "NO", "yes", "Durable Goods",
  "Durable Goods New Orders (Monthly, Seasonally Adjusted)", "$", "B", 0.001, 1,
  "The value of new orders placed with U.S. factories for durable goods, items meant to last three years or more such as machinery, vehicles, aircraft and appliances, in billions of dollars per month. Orders are an early signal of future production, but they swing widely because a few large aircraft or defense orders can dominate a month. This is the advance report, so figures are revised.",

  "trade", "ftd", "BOPGS", "BAL", "yes", "Trade Balance",
  "Goods & Services Trade Balance (Monthly, Seasonally Adjusted)", "$", "B", 0.001, 1,
  "U.S. exports minus imports of goods and services, in billions of dollars per month. A negative number is a trade deficit: the U.S. bought more from other countries than it sold to them. Seasonally adjusted, and published with roughly a one-month lag compared with the other tabs."
)

start_month <- format(seq(Sys.Date(), by = "-60 months", length.out = 2)[2], "%Y-%m")  # ~5 years of history

query_eits <- function(program, cat, dtype, adj, from) {
  if (!nzchar(census_key)) stop("CENSUS_API_KEY is empty. Run Sys.setenv(CENSUS_API_KEY = 'your-key') first.", call. = FALSE)
  fetch <- function(...) {
    request(paste0("https://api.census.gov/data/timeseries/eits/", program)) |>
      req_url_query(
        get = "cell_value,error_data,geo_level_code,time_slot_id",
        category_code = cat, data_type_code = dtype, seasonally_adj = adj,
        time = paste("from", from), key = census_key, ...
      ) |>
      req_error(is_error = \(x) FALSE) |>
      req_perform()
  }
  body_of <- function(r) if (resp_has_body(r)) resp_body_string(r) else ""
  r <- fetch()
  body <- body_of(r)
  if (resp_status(r) == 400 && grepl("missing 'for'", body)) {  # some programs need a geography
    r <- fetch(`for` = "us:*")
    body <- body_of(r)
  }
  if (resp_status(r) != 200 || !nzchar(body)) {
    hint <- if (resp_status(r) == 204 || !nzchar(body)) " (no rows: these codes match nothing; use explore())" else ""
    stop("HTTP ", resp_status(r), hint, " ", substr(body, 1, 200), call. = FALSE)
  }
  m <- fromJSON(body)
  df <- as_tibble(setNames(as.data.frame(m[-1, , drop = FALSE]), m[1, ]))
  if ("geo_level_code" %in% names(df)) {
    geos <- unique(df$geo_level_code)
    if ("US" %in% geos) {
      df <- filter(df, geo_level_code == "US")
    } else if (length(geos) > 1) {
      stop("several geographies (", paste(geos, collapse = ", "), "); none is 'US'", call. = FALSE)
    }
  }
  out <- df |>
    filter(grepl("^\\d{4}-\\d{2}$", time), error_data == "no") |>
    transmute(date = as.Date(paste0(time, "-01")),
              value = suppressWarnings(as.numeric(cell_value)))
  if (anyDuplicated(out$date)) {
    warning(program, "/", cat, "/", dtype, ": several values per month, keeping the last. Check the codes.", call. = FALSE)
  }
  out |> group_by(date) |> slice_tail(n = 1) |> ungroup() |> arrange(date)
}

# explore("resconst") lists every category_code / data_type_code / seasonally_adj combination
# a program offers for one recent month, with its value so you can sanity-check magnitudes.
# Returns a tibble: use View(explore("resconst")) or filter it. Quarterly programs need
# month = "2025-Q2" style values.
explore <- function(program, month = NULL, geo = NULL) {
  if (!nzchar(census_key)) stop("Set CENSUS_API_KEY first.", call. = FALSE)
  try_months <- if (is.null(month)) format(seq(Sys.Date(), by = "-1 month", length.out = 9)[4:9], "%Y-%m") else month
  for (mo in try_months) {
    r <- request(paste0("https://api.census.gov/data/timeseries/eits/", program)) |>
      req_url_query(get = "category_code,data_type_code,seasonally_adj,cell_value,error_data,geo_level_code,time_slot_id",
                    time = mo, key = census_key, `for` = geo) |>
      req_error(is_error = \(x) FALSE) |>
      req_perform()
    body <- if (resp_has_body(r)) resp_body_string(r) else ""
    if (resp_status(r) == 200 && nzchar(body)) {
      m <- fromJSON(body)
      message("Showing ", mo, " (", nrow(m) - 1, " rows)")
      return(as_tibble(setNames(as.data.frame(m[-1, , drop = FALSE]), m[1, ])) |>
               arrange(category_code, data_type_code, seasonally_adj))
    }
  }
  stop("No data returned for ", program, ". Check the program name and your key.", call. = FALSE)
}

# Dry run: tests every row of `series` and reports status, history length, and the latest
# raw value. Writes nothing.
check_series <- function() {
  bind_rows(lapply(seq_len(nrow(series)), function(i) {
    s <- series[i, ]
    tryCatch({
      d <- query_eits(s$program, s$cat, s$dtype, s$adj, start_month)
      tibble(key = s$key, ok = nrow(d) > 0, n = nrow(d), first = min(d$date), last = max(d$date),
             latest = tail(d$value, 1), note = "")
    }, error = function(e) {
      tibble(key = s$key, ok = FALSE, n = 0L, first = as.Date(NA), last = as.Date(NA),
             latest = NA_real_, note = conditionMessage(e))
    })
  }))
}

main <- function() {
  out <- lapply(seq_len(nrow(series)), function(i) {
    s <- series[i, ]
    tryCatch({
      d <- query_eits(s$program, s$cat, s$dtype, s$adj, start_month)
      if (!nrow(d)) stop("no rows returned")
      message(sprintf("%-9s latest %s = %s", s$key, format(tail(d$date, 1)), tail(d$value, 1)))
      list(key = s$key, tab = s$tab, title = s$title, desc = s$desc, prefix = s$prefix, suffix = s$suffix,
           decimals = s$dec, dates = format(d$date), values = round(d$value * s$scale, 3))
    }, error = function(e) {
      message("SKIPPED ", s$key, ": ", conditionMessage(e))
      NULL
    })
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) stop("No Census series loaded; keeping previous census_data.json")
  writeLines(toJSON(list(updated = format(Sys.Date()), series = out), auto_unbox = TRUE, na = "null"),
             "census_data.json")
  message("Wrote census_data.json with ", length(out), " series")
}

if (!interactive()) main()
