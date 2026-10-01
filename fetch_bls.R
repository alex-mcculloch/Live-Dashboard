library(httr2)
library(dplyr)
library(lubridate)
library(jsonlite)

# Free key: https://data.bls.gov/registrationEngine/  (renew yearly)
bls_key <- Sys.getenv("BLS_API_KEY")

this_year <- year(Sys.Date())

# LNS14000000 = Unemployment rate, seasonally adjusted (CPS)
# CUUR0000SA0 = CPI-U, all items, U.S. city average, not seasonally adjusted
series_ids <- c("LNS14000000", "CUUR0000SA0")

resp <- request("https://api.bls.gov/publicAPI/v2/timeseries/data/") |>
  req_body_json(list(
    seriesid        = as.list(series_ids),
    startyear       = as.character(this_year - 3),  # extra history so YoY works
    endyear         = as.character(this_year),
    registrationkey = bls_key
  )) |>
  req_perform() |>
  resp_body_json()

if (!identical(resp$status, "REQUEST_SUCCEEDED")) {
  stop("BLS request failed: ", paste(unlist(resp$message), collapse = " | "))
}

# Turn one series from the response into a tidy monthly tibble.
# BLS returns "-" for months with no data; those become NA.
parse_series <- function(s) {
  bind_rows(lapply(s$data, function(d) {
    tibble(
      year   = as.integer(d$year),
      period = d$period,
      value  = suppressWarnings(as.numeric(d$value))
    )
  })) |>
    filter(grepl("^M(0[1-9]|1[0-2])$", period)) |>  # drop M13 (annual avg)
    mutate(date = make_date(year, as.integer(substr(period, 2, 3)), 1)) |>
    arrange(date)
}

by_id <- setNames(resp$Results$series, vapply(resp$Results$series, `[[`, "", "seriesID"))

# Unemployment rate: last 24 months
unrate <- parse_series(by_id[["LNS14000000"]]) |> tail(24)

# CPI: year-over-year % change, matched on date so a missing month can't shift the lag
cpi_raw <- parse_series(by_id[["CUUR0000SA0"]])
cpi <- cpi_raw |>
  left_join(
    cpi_raw |> transmute(date = date %m+% years(1), prev = value),
    by = "date"
  ) |>
  mutate(value = (value / prev - 1) * 100) |>
  filter(!is.na(prev) | !is.na(value)) |>
  tail(24)

pack <- function(df) list(dates = format(df$date), values = round(df$value, 1))

out <- list(
  updated = format(Sys.Date()),
  UNRATE  = pack(unrate),
  CPI     = pack(cpi)
)

json <- toJSON(out, auto_unbox = TRUE, na = "null")
writeLines(json, "econ_data.json")
message("Wrote econ_data.json")
