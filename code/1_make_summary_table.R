## make table 1 and table A1: summary statistics for key variables by metro status, BEA region, and state

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

## load data ----
dat_xc_county <- read_rds(file = "../final_data/fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3_countyrow.rds")
dat_xc_county <- dat_xc_county %>%
  group_by(state, cs_name) %>%
  mutate(metroname_mode = Mode(metroname))
dat_xc_countyset <- dat_xc_county %>%
  distinct(state, cs_name, .keep_all = TRUE) %>%
  select(-county_code, -county_fips, -county_name, -metroname)

negative_excess_death <- dat_xc_countyset %>%
  filter(excess_deaths_2020 < 0)
## exclude counties with negative excess mortality
dat_xc_countyset <- dat_xc_countyset %>%
  filter(excess_deaths_2020 >= 0)

## Prep data to calculate weighted mean ----
dat_xc_countyset_sum <- dat_xc_countyset %>%
  ungroup() %>%
  transmute(
    year, state, cs_name,
    region_bea, region_group_bea, metroname_mode, cens_pop_est,
    fitted_deaths_2020,
    all_cause_deaths_2020,
    actual_pred_ratio = all_cause_deaths_2020 / fitted_deaths_2020,
    excess_deaths_2020,
    excess_death_rate_2020 = excess_death_rate_2020 * 100,
    covid_deaths_2020,
    covid_excess_ratio = covid_deaths_2020 / excess_deaths_2020
  )

## All ----
mean_all <- dat_xc_countyset_sum %>%
  summarise(across(c(
    "covid_deaths_2020", "fitted_deaths_2020",
    "all_cause_deaths_2020", "cens_pop_est"
  ), ~ sum(.x, na.rm = TRUE)),
  n = n()
  ) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    unassigned_deaths = excess_deaths_2020 - covid_deaths_2020
  ) %>%
  mutate(
    excess_death_rate = excess_deaths_2020 / offset,
    prop_assigned = covid_deaths_2020 / excess_deaths_2020 * 100,
    obs_exp_ratio = all_cause_deaths_2020 / fitted_deaths_2020,
    col1 = "All"
  ) %>%
  select(
    col1, fitted_deaths_2020, all_cause_deaths_2020,
    obs_exp_ratio, excess_deaths_2020, excess_death_rate,
    covid_deaths_2020, prop_assigned, n
  )

## Metro ----
mean_metro <- dat_xc_countyset_sum %>%
  group_by(metroname_mode) %>%
  summarise(across(c(
    "covid_deaths_2020", "fitted_deaths_2020",
    "all_cause_deaths_2020", "cens_pop_est"
  ), ~ sum(.x, na.rm = TRUE)),
  n = n()
  ) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    unassigned_deaths = excess_deaths_2020 - covid_deaths_2020
  ) %>%
  mutate(
    excess_death_rate = excess_deaths_2020 / offset,
    prop_assigned = covid_deaths_2020 / excess_deaths_2020 * 100,
    obs_exp_ratio = all_cause_deaths_2020 / fitted_deaths_2020
  ) %>%
  select(
    metroname_mode, fitted_deaths_2020, all_cause_deaths_2020,
    obs_exp_ratio, excess_deaths_2020, excess_death_rate,
    covid_deaths_2020, prop_assigned, n
  )
colnames(mean_metro)[1] <- "col1"

## BEA region ----
mean_region <- dat_xc_countyset_sum %>%
  group_by(region_bea) %>%
  summarise(across(c(
    "covid_deaths_2020", "fitted_deaths_2020",
    "all_cause_deaths_2020", "cens_pop_est"
  ), ~ sum(.x, na.rm = TRUE)),
  n = n()
  ) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    unassigned_deaths = excess_deaths_2020 - covid_deaths_2020
  ) %>%
  mutate(
    excess_death_rate = excess_deaths_2020 / offset,
    prop_assigned = covid_deaths_2020 / excess_deaths_2020 * 100,
    obs_exp_ratio = all_cause_deaths_2020 / fitted_deaths_2020
  ) %>%
  select(
    region_bea, fitted_deaths_2020, all_cause_deaths_2020,
    obs_exp_ratio, excess_deaths_2020, excess_death_rate,
    covid_deaths_2020, prop_assigned, n
  )
colnames(mean_region)[1] <- "col1"

## BEA region * metro ----
mean_region_metro <- dat_xc_countyset_sum %>%
  group_by(region_bea, metroname_mode) %>%
  summarise(across(c(
    "covid_deaths_2020", "fitted_deaths_2020",
    "all_cause_deaths_2020", "cens_pop_est"
  ), ~ sum(.x, na.rm = TRUE)),
  n = n()
  ) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    unassigned_deaths = excess_deaths_2020 - covid_deaths_2020
  ) %>%
  mutate(
    excess_death_rate = excess_deaths_2020 / offset,
    prop_assigned = covid_deaths_2020 / excess_deaths_2020 * 100,
    obs_exp_ratio = all_cause_deaths_2020 / fitted_deaths_2020
  ) %>%
  select(
    region_bea, metroname_mode, fitted_deaths_2020, all_cause_deaths_2020,
    obs_exp_ratio, excess_deaths_2020, excess_death_rate,
    covid_deaths_2020, prop_assigned, n
  )
colnames(mean_region_metro)[1] <- "col1"


## State ----
mean_state <- dat_xc_countyset_sum %>%
  group_by(state) %>%
  summarise(across(c(
    "covid_deaths_2020", "fitted_deaths_2020",
    "all_cause_deaths_2020", "cens_pop_est"
  ), ~ sum(.x, na.rm = TRUE)),
  n = n()
  ) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    unassigned_deaths = excess_deaths_2020 - covid_deaths_2020
  ) %>%
  mutate(
    excess_death_rate = excess_deaths_2020 / offset,
    prop_assigned = covid_deaths_2020 / excess_deaths_2020 * 100,
    obs_exp_ratio = all_cause_deaths_2020 / fitted_deaths_2020
  ) %>%
  select(
    state, fitted_deaths_2020, all_cause_deaths_2020,
    obs_exp_ratio, excess_deaths_2020, excess_death_rate,
    covid_deaths_2020, prop_assigned, n
  )
colnames(mean_state)[1] <- "col1"

## combine all in one table ----
mean_table <- bind_rows(mean_all, mean_metro, mean_region, mean_region_metro, mean_state) %>%
  mutate(
    fitted_deaths_2020 = round(fitted_deaths_2020, 0),
    excess_deaths_2020 = round(excess_deaths_2020, 0),
    excess_death_rate = round(excess_death_rate, 1),
    prop_assigned = round(prop_assigned, 1),
    obs_exp_ratio = round(obs_exp_ratio, 2)
    # row = row_number(),
    # col1 = ifelse(row == 8 & col1 == "Great Lakes", "Great Lakes ", col1)
  ) %>%
  select(col1, metroname_mode, everything())
mean_table <- as.data.frame(mean_table)

kt <- kbl(mean_table, "latex",
  booktabs = T, linesep = "",
  col.names = c(
    "BEA Region", "Metro-Nonmetro Status",
    "Predicted", "Actual", "Predicted/Expected",
    "Excess",
    "Excess per 1000",
    "Covid-19",
    "Proportion", "N"
  ),
  format.args = list(big.mark = ","),
  row.names = TRUE
)
kt <- gsub(".*midrule", "", kt)
kt <- gsub("bottomrule.*$", "", kt)
table_out <- substr(kt, 1, nchar(kt) - 1)
save_kable(table_out, file = paste0(outpath, "tab1_input.tex"), keep_tex = TRUE)
