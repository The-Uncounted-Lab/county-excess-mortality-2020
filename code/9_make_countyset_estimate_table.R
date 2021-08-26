## make appendix Table C1: summary stat for each county set

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

## load data ----
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
tab_c1 <- dat_xc %>%
  mutate(
    excess_deaths_lwr_ci = all_cause_deaths_2020 - pred_deaths_upr_ci_2020,
    excess_deaths_upr_ci = all_cause_deaths_2020 - pred_deaths_lwr_ci_2020,
    cs_name = ifelse(cens_pop_est == 220164, "Doña Ana", cs_name),
    cs_name = str_replace_all(cs_name, "\\+", "-")
  ) %>%
  transmute(
    county = paste0(cs_name, ", ", state),
    expected_deaths_2020 = round(fitted_deaths_2020, 0),
    expected_deaths_ci_2020 = paste0("(", round(pred_deaths_lwr_ci_2020, 0), ",", round(pred_deaths_upr_ci_2020, 0), ")"),
    all_cause_deaths_2020 = round(all_cause_deaths_2020, 0),
    obs_exp_ratio = round(all_cause_deaths_2020 / expected_deaths_2020, 2),
    excess_deaths_2020 = round(excess_deaths_2020, 0),
    excess_deaths_ci_2020 = paste0("(", round(excess_deaths_lwr_ci, 0), ",", round(excess_deaths_upr_ci, 0), ")"),
    excess_death_rate_2020 = round(excess_death_rate_2020 * 100, 0),
    covid_deaths_2020,
    covid_excess_ratio = round(covid_deaths_2020 / excess_deaths_2020 * 100, 1),
    covid_excess_ratio = ifelse(
      covid_excess_ratio < 0 | is.infinite(covid_excess_ratio), NA, covid_excess_ratio
    )
  )
tab_c1 <- as.data.frame(tab_c1)

kt <- kable(tab_c1,
  "latex",
  booktabs = T, longtable = TRUE, linesep = "",
  row.names = FALSE
) %>%
  column_spec(1, width = "7em")

kt <- gsub(".*midrule", "", kt)
kt <- gsub("bottomrule.*$", "", kt)
save_kable(kt, file = paste0(outpath, "atab_c1_county_estimate_input.tex"), keep_tex = TRUE)
