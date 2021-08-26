## make appendix tab A2: cumulative pop in diff levels of excess death assignmnent to COVID-19

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
covid_excess_ratio <- dat_xc %>%
  transmute(
    county = cs_name, cens_pop_est,
    covid_excess_death_ratio = (covid_deaths_2020 / excess_deaths_2020) * 100,
    covid_excess_death_ratio = ifelse(
      excess_deaths_2020 < 0, NA, ifelse(
        covid_excess_death_ratio > 100, 100, covid_excess_death_ratio
      )
    ),
    covid_excess_death_ratio_cat = cut(covid_excess_death_ratio, breaks = seq(0, 100, 5), include.lowest = TRUE)
  )

covid_excess_ratio_cum <- covid_excess_ratio %>%
  group_by(covid_excess_death_ratio_cat) %>%
  summarise(
    n = n(),
    pop = sum(cens_pop_est)
  ) %>%
  arrange(covid_excess_death_ratio_cat) %>%
  mutate(cum_pop = cumsum(pop),
         sum_pop = sum(pop),
         perc = round(cum_pop/sum_pop * 100, 2),
         perc = paste0(perc, "%")) %>%
  select(-sum_pop)

kt <- kbl(covid_excess_ratio_cum, "latex",
  booktabs = T, linesep = "",
  col.names = c(
    "Percent of excess deaths \n assigned to COVID-19",
    "Number of county sets",
    "Population",
    "Cumulative population",
    "Cumulative percent"
  ),
  format.args = list(big.mark = ","),
  row.names = TRUE
)
save_kable(kt, file = paste0(outpath, "atab_a2_covid_excess_ratio_pop.tex"), keep_tex = TRUE)
