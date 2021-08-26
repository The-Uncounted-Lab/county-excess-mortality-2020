## make Fig 2 and 3: county-level map with horizontal bars at bottom

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

## Prep data to make horizontal bar charts ----
## 20 counties with highest excess mortality and lowest COVID-19 to excess ratio
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
dat_xc <- dat_xc %>%
  transmute(
    state, cs_name, cens_pop_est,
    county_name = paste0(cs_name, ", ", state),
    all_cause_death_rate_2020 = all_cause_death_rate_2020 * 100,
    covid_excess_ratio = covid_deaths_2020 / excess_deaths_2020,
    actual_pred_ratio = all_cause_death_rate_2020 / fitted_death_rate_2020,
    fitted_death_rate_2020 = fitted_death_rate_2020 * 100,
    pred_death_rate_lwr_ci_2020 = pred_death_rate_lwr_ci_2020 * 100,
    pred_death_rate_upr_ci_2020 = pred_death_rate_upr_ci_2020 * 100,
    pred_death_rate_std_err = pred_death_rate_std_err * 100,
    excess_death_rate_2020 = excess_death_rate_2020 * 100,
    excess_assigned_covid_death_rate = excess_assigned_covid_death_rate * 100,
    excess_unassigned_covid_death_rate = excess_unassigned_covid_death_rate * 100,
    excess_ci_low = all_cause_death_rate_2020 - pred_death_rate_upr_ci_2020,
    excess_ci_high = all_cause_death_rate_2020 - pred_death_rate_lwr_ci_2020,
    excess_unassigned_ci_low = excess_ci_low - excess_assigned_covid_death_rate,
    excess_unassigned_ci_high = excess_ci_high - excess_assigned_covid_death_rate
  )

ncounties <- 20
pop_cutoff <- 200000
p1_highest_excess_death <- dat_xc %>%
  filter(cens_pop_est > pop_cutoff) %>%
  slice_max(order_by = excess_death_rate_2020, n = ncounties) %>%
  mutate(category = "Highest excess mortality")
p3_underreporting <- dat_xc %>%
  filter(cens_pop_est > pop_cutoff, covid_excess_ratio > 0) %>%
  slice_min(order_by = covid_excess_ratio, n = ncounties) %>%
  mutate(category = "Lowest COVID-19 to excess ratio")

cases_panel <- bind_rows(
  p1_highest_excess_death,
  p3_underreporting
) %>%
  select(
    category, county_name,
    excess_assigned_covid_death_rate, excess_unassigned_covid_death_rate
  ) %>%
  gather(variable, value, -county_name, -category) %>%
  arrange(county_name) %>%
  mutate(
    variable = case_when(
      variable == "excess_assigned_covid_death_rate" ~ "Excess deaths assigned to COVID-19",
      variable == "excess_unassigned_covid_death_rate" ~ "Excess deaths not assigned to COVID-19"
    ),
    variable = factor(variable,
      levels = c("Excess deaths not assigned to COVID-19", "Excess deaths assigned to COVID-19")
    )
  )
cases_panel <- left_join(cases_panel,
  dat_xc %>% ungroup() %>%
    transmute(
      county_name, cens_pop_est,
      excess_death_rate_2020,
      pred_death_rate_upr_ci_2020,
      pred_death_rate_lwr_ci_2020,
      excess_ci_low = all_cause_death_rate_2020 - pred_death_rate_upr_ci_2020,
      excess_ci_high = all_cause_death_rate_2020 - pred_death_rate_lwr_ci_2020
    ) %>%
    select(-pred_death_rate_lwr_ci_2020, -pred_death_rate_upr_ci_2020),
  by = "county_name"
)
cases_panel <- cases_panel %>%
  mutate_at(
    vars(excess_ci_low, excess_ci_high, excess_death_rate_2020),
    ~ ifelse(variable == "Excess deaths assigned to COVID-19", ., NA)
  ) %>%
  mutate(
    category = factor(category,
      levels = c("Highest excess mortality", "Lowest COVID-19 to excess ratio")
    ),
    county_name = case_when(
      county_name == "Accomack+Northampton+Virginia Beach (city), VA" ~ "Accomack+Northampton \n +Virginia Beach (city), VA",
      cens_pop_est == 220164 ~ "Doña Ana, NM",
      TRUE ~ county_name
    )
  )


## Prep data to make county-level maps ----
## plot excess death rate and excess deaths not assigned to COVID-19 ratio

dat_xc_county <- read_rds(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3_countyrow.rds"))
state_coord <- usmap::us_map()
county_coord <- county_map %>%
  mutate(fips_code = as.numeric(id))

excess_death <- dat_xc_county %>%
  transmute(
    county = county_name,
    fips_code = county_code,
    excess_death_rate_2020 = excess_death_rate_2020 * 100,
    excess_death_rate_q25 = quantile(excess_death_rate_2020, probs = 0.25),
    excess_death_rate_q50 = quantile(excess_death_rate_2020, probs = 0.5),
    excess_death_rate_q75 = quantile(excess_death_rate_2020, probs = 0.75)
  )
excess_death_map_df <-
  left_join(
    county_coord,
    excess_death,
    by = "fips_code"
  )
excess_death_map_df <- excess_death_map_df %>%
  mutate(
    excess_death_rate_qcat = case_when(
      excess_death_rate_2020 < excess_death_rate_q25 ~ "<25th percentile",
      excess_death_rate_2020 >= excess_death_rate_q25 & excess_death_rate_2020 < excess_death_rate_q50 ~ "25-50th percentile",
      excess_death_rate_2020 >= excess_death_rate_q50 & excess_death_rate_2020 < excess_death_rate_q75 ~ "50-75th percentile",
      excess_death_rate_2020 >= excess_death_rate_q75 ~ ">75th percentile",
      is.na(excess_death_rate_2020) ~ "Missing data"
    ),
    excess_death_rate_qcat = factor(
      excess_death_rate_qcat,
      levels = c(">75th percentile", "50-75th percentile", "25-50th percentile", "<25th percentile", "Missing data")
    )
  )

## Fig 2 ----
fig2_map <- ggplot() +
  geom_polygon(
    data = excess_death_map_df,
    aes(x = long, y = lat, group = group, fill = excess_death_rate_qcat),
    color = "gray30", size = 0.05
  ) +
  geom_polygon(
    data = state_coord,
    aes(x = x, y = y, group = group),
    color = "gray30", fill = NA, size = 0.4
  ) +
  coord_equal() +
  theme_map() +
  labs(fill = "Excess death rate") +
  scale_fill_manual(values = c("#08519c", "#3182bd", "#6baed6", "#bdd7e7", "gray70")) +
  theme(
    legend.position = c(0.16, -0.01),
    strip.background = element_blank(),
    legend.direction = "horizontal"
  )

fig2_bar <- ggplot(
  cases_panel %>%
    filter(category == "Highest excess mortality"),
  aes(reorder_within(county_name, -value, category), value, fill = variable)
) +
  geom_bar(
    stat = "identity", position = "stack", width = 0.8
  ) +
  geom_errorbar(aes(ymin = excess_ci_low, ymax = excess_ci_high),
    width = 0.2, size = 0.5, color = "#08306B",
    position = "identity"
  ) +
  scale_fill_manual(values = rev(blue_palette[1:2])) +
  scale_x_reordered() +
  labs(
    x = "", y = "Deaths (Per 100,000 Person-Years)",
    title = "Highest excess mortality"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  plot_theme() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, size = 8, hjust = 0.95),
    axis.title.y = element_text(size = 10),
    legend.position = c(0.8, 0.9)
  )
fig2 <- grid.arrange(
  fig2_map, fig2_bar,
  layout_matrix = rbind(
    c(1, 1, 1, 1, 1),
    c(1, 1, 1, 1, 1),
    c(2, 2, 2, 2, 2)
  )
)
ggsave(fig2, file = paste0(outpath, "fig2_excess_death_map_bar.png"), width = 8, height = 10)


## Fig 4 ----
excess_covid_death <- dat_xc_county %>%
  transmute(
    county = county_name,
    fips_code = county_code,
    covid_excess_death_ratio = (covid_deaths_2020 / excess_deaths_2020) * 100,
    noncovid_excess_death_ratio = (1 - covid_deaths_2020 / excess_deaths_2020) * 100,
    noncovid_excess_death_ratio = ifelse(
      excess_deaths_2020 < 0 | excess_unassigned_covid_deaths < 0, 0, noncovid_excess_death_ratio
    )
  )
excess_covid_death_map_df <-
  left_join(county_coord,
    excess_covid_death,
    by = "fips_code"
  )
excess_covid_death_map_df <- excess_covid_death_map_df %>%
  mutate(
    noncovid_excess_death_percent_cat = case_when(
      noncovid_excess_death_ratio <= 10 ~ "Low [0,10]",
      noncovid_excess_death_ratio > 10 & noncovid_excess_death_ratio <= 25 ~ "Moderate (10,25]",
      noncovid_excess_death_ratio > 25 ~ "High >25",
      is.na(noncovid_excess_death_ratio) ~ "Missing data"
    ),
    noncovid_excess_death_percent_cat = factor(
      noncovid_excess_death_percent_cat,
      levels = c("High >25", "Moderate (10,25]", "Low [0,10]", "Missing data")
    )
  )

fig3_map <- ggplot() +
  geom_polygon(
    data = excess_covid_death_map_df,
    aes(x = long, y = lat, group = group, fill = noncovid_excess_death_percent_cat),
    color = "gray30", size = 0.05
  ) +
  geom_polygon(
    data = state_coord,
    aes(x = x, y = y, group = group),
    color = "gray30", fill = NA, size = 0.4
  ) +
  coord_equal() +
  theme_map() +
  labs(fill = "Percent of excess deaths \n not assigned to COVID-19") +
  scale_fill_manual(values = c(rev(blue_palette), "gray80")) +
  theme(
    legend.position = c(0.25, -0.01),
    strip.background = element_blank(),
    legend.direction = "horizontal"
  )

fig3_bar <- ggplot(
  cases_panel %>%
    filter(category == "Lowest COVID-19 to excess ratio"),
  aes(reorder_within(county_name, -value, category), value, fill = variable)
) +
  geom_bar(
    stat = "identity", position = "stack", width = 0.8
  ) +
  geom_errorbar(aes(ymin = excess_ci_low, ymax = excess_ci_high),
    width = 0.2, size = 0.5, color = "#08306B",
    position = "identity"
  ) +
  scale_fill_manual(values = rev(blue_palette[1:2])) +
  scale_x_reordered() +
  labs(
    x = "", y = "Deaths (Per 100,000 Person-Years)",
    title = "Lowest COVID-19 to excess ratio"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  plot_theme() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, size = 8, hjust = 0.95),
    axis.title.y = element_text(size = 10),
    legend.position = c(0.8, 0.9)
  )
fig3 <- grid.arrange(
  fig3_map, fig3_bar,
  layout_matrix = rbind(
    c(1, 1, 1, 1, 1),
    c(1, 1, 1, 1, 1),
    c(2, 2, 2, 2, 2)
  )
)
ggsave(fig3, file = paste0(outpath, "fig3_excess_not_covid_map_bar.png"), width = 8, height = 10)
