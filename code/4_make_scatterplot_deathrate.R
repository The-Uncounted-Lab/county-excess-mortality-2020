## make Fig 4: scatterplot of excess death rate vs. COVID-19 death rate

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

# load data ----
dat_xc_county <- read_rds(file = "../final_data/fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3_countyrow.rds")
dat_xc_county <- dat_xc_county %>%
  group_by(state, cs_name) %>%
  mutate(metroname_mode = Mode(metroname))
dat_xc_countyset <- dat_xc_county %>%
  distinct(state, cs_name, .keep_all = TRUE) %>%
  select(-county_code, -county_fips, -county_name, -metroname)
dat_xc_countyset <- dat_xc_countyset %>%
  transmute(
    state, cs_name, cens_pop_est, region_bea, region_group_bea, metroname_mode,
    excess_death_rate_2020 = excess_death_rate_2020 * 100,
    covid_death_rate_2020 = covid_death_rate_2020 * 100,
    excess_unassigned_covid_death_rate = excess_unassigned_covid_death_rate * 100
  )

# make plot ----
ggplot(
  dat_xc_countyset %>%
    group_by(region_bea) %>%
    mutate(
      q99 = quantile(covid_death_rate_2020, 0.99),
      covid_death_rate_2020 = ifelse(covid_death_rate_2020 > q99, NA, covid_death_rate_2020),
      excess_death_rate_2020 = ifelse(covid_death_rate_2020 > q99, NA, excess_death_rate_2020)
    ),
  aes(covid_death_rate_2020, excess_death_rate_2020, weight = cens_pop_est)
) +
  geom_point(aes(color = metroname_mode, size = cens_pop_est), alpha = 0.7) +
  scale_color_brewer(palette = "RdYlBu") +
  facet_wrap(~region_bea, ncol = 2) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed", color = "gray50") +
  labs(
    x = "COVID-19 deaths per 100,000 population",
    y = "Excess deaths per 100,000 population "
  ) +
  plot_theme() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11)
  ) +
  guides(size = "none")
ggsave(paste0(outpath, "fig4_covid_excess_rate_scatterplot_region.png"), width = 8, height = 11)
