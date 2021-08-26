## make Fig 5: excess death not assigned to COVID-19 of selected counties

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

# load data
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
dat_xc <- dat_xc %>%
  transmute(
    state, cs_name, cens_pop_est,
    county_name = paste0(cs_name, ", ", state),
    all_cause_death_rate_2020 = all_cause_death_rate_2020 * 100,
    pred_death_rate_lwr_ci_2020 = pred_death_rate_lwr_ci_2020 * 100,
    pred_death_rate_upr_ci_2020 = pred_death_rate_upr_ci_2020 * 100,
    pred_death_rate_std_err = pred_death_rate_std_err * 100,
    excess_assigned_covid_death_rate = excess_assigned_covid_death_rate * 100,
    excess_unassigned_covid_death_rate = excess_unassigned_covid_death_rate * 100,
    excess_ci_low = all_cause_death_rate_2020 - pred_death_rate_upr_ci_2020,
    excess_ci_high = all_cause_death_rate_2020 - pred_death_rate_lwr_ci_2020,
    excess_unassigned_ci_low = excess_ci_low - excess_assigned_covid_death_rate,
    excess_unassigned_ci_high = excess_ci_high - excess_assigned_covid_death_rate,
    county_name = case_when(
      cens_pop_est == 220164 ~ "Doña Ana, NM",
      county_name == "Barnstable+Dukes+Nantucket, MA" ~ "Barnstable+Dukes+\nNantucket, MA",
      TRUE ~ county_name
    )
  )

ncounties <- 20
pop_cutoff <- 200000
p1_highest <- dat_xc %>%
  filter(cens_pop_est > pop_cutoff) %>%
  slice_max(order_by = excess_unassigned_covid_death_rate, n = ncounties) %>%
  mutate(category = "Highest excess deaths not assigned to COVID-19")
p2_lowest <- dat_xc %>%
  filter(cens_pop_est > pop_cutoff) %>%
  slice_min(order_by = excess_unassigned_covid_death_rate, n = ncounties) %>%
  mutate(category = "Lowest excess deaths not assigned to COVID-19")
p3_strictly_negative <- dat_xc %>%
  filter(
    cens_pop_est > pop_cutoff,
    excess_unassigned_ci_high < 0
  ) %>%
  slice_min(order_by = excess_unassigned_covid_death_rate, n = ncounties) %>%
  mutate(category = "Strictly negative excess deaths not assigned to COVID-19")

cases_panel <- bind_rows(
  p1_highest,
  p2_lowest,
  p3_strictly_negative
) %>%
  select(
    category, county_name,
    excess_unassigned_covid_death_rate, excess_unassigned_ci_low, excess_unassigned_ci_high
  )

ggplot(
  cases_panel %>%
    filter(
      category != "Lowest excess deaths not assigned to COVID-19",
      county_name != "Butte, CA"
    ),
  aes(reorder(county_name, excess_unassigned_covid_death_rate), excess_unassigned_covid_death_rate)
) +
  geom_bar(
    stat = "identity", position = "stack", width = 0.8, fill = blue_palette[2]
  ) +
  geom_errorbar(aes(ymin = excess_unassigned_ci_low, ymax = excess_unassigned_ci_high),
    width = 0.2, size = 0.5, color = "#08306B",
    position = "identity"
  ) +
  facet_wrap(~category, scales = "free_y", ncol = 1) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = "", y = "Excess death rate not assigned to COVID-19 (Per 100,000 Person-Years)") +
  guides(fill = guide_legend(reverse = TRUE)) +
  plot_theme() +
  theme(
    strip.text = element_text(size = 11),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(0, 5, 0, 0, "mm")
  )
ggsave(paste0(outpath, "fig5_excess_not_covid_strict_negative.png"), width = 8, height = 10)
