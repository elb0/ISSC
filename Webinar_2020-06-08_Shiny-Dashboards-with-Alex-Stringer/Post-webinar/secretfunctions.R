# Get recovered
recovered <- readr::read_csv(
  file = "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/recovered_cumulative.csv",
  col_names = TRUE,
  col_types = c("ccn")
) %>%
  dplyr::select(
    date_recovered,
    province,
    cumulative_recovered
  ) %>%
  mutate(date_recovered = lubridate::dmy(date_recovered))

# Get mortality
mortality <- readr::read_csv(
  file = "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv",
  col_names = TRUE,
  col_types = c("cccccccccccc")
) %>%
  dplyr::select(
    death_id,
    province,
    date_death_report
  ) %>%
  mutate(date_death_report = lubridate::dmy(date_death_report))

# Daily mortality
mortality_daily <- mortality %>%
  group_by(province,date_death_report) %>%
  summarize(deaths = n())

# Daily recovered
recovered_daily <- recovered %>%
  arrange(province,date_recovered) %>%
  group_by(province) %>%
  mutate(recovered = cumulative_recovered - lag(cumulative_recovered,1)) %>%
  filter(!is.na(recovered)) # Remove the first row, which does not have a value

