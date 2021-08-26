## make appendix figure A2 and A3: time series plots

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

# load data
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
dat_pan <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2011_2020_W2020_wash_6_3.csv"))

bea <- read_rds("../raw_data/state_bearegion_crosswalk.rds")
bea <- bea %>%
  transmute(
    state = state_abb,
    region_bea, region_group_bea
  )
dat_xc <- left_join(
  dat_xc, bea,
  by = "state"
)

# select 4 most populous counties from each BEA region
selected_counties <- dat_xc %>%
  group_by(region_bea) %>%
  slice_max(order_by = cens_pop_est, n = 4)

plot_df <- dat_pan %>%
  transmute(
    cs_name, state, year,
    total_death_rate, fitted_death_rate,
    pred_death_rate_lwr_ci, pred_death_rate_upr_ci
  ) %>%
  right_join(
    .,
    selected_counties %>%
      transmute(cs_name, state, region_bea, cens_pop_est, covid_death_rate_2020),
    by = c("cs_name", "state")
  ) %>%
  mutate_at(
    vars(matches("rate")),
    ~ . * 100
  ) %>%
  mutate(
    predicted_and_covid = fitted_death_rate + covid_death_rate_2020,
    cs_name = paste0(cs_name, " County, ", state),
    cs_name = ifelse(cs_name == "New York (county) County, NY", "New York County, NY", ifelse(
      cs_name == "St. Louis (county) County, MO", "St. Louis County, MO", cs_name
    ))
  )

plot_df_long <- plot_df %>%
  select(
    region_bea, cs_name, year, total_death_rate, fitted_death_rate,
    pred_death_rate_lwr_ci, pred_death_rate_upr_ci, predicted_and_covid
  ) %>%
  gather(
    variable, value, -region_bea, -cs_name, -year,
    -pred_death_rate_lwr_ci, -pred_death_rate_upr_ci
  ) %>%
  arrange(region_bea, cs_name) %>%
  mutate_at(
    vars(pred_death_rate_lwr_ci, pred_death_rate_upr_ci),
    ~ ifelse(variable == "total_death_rate", NA, .)
  ) %>%
  mutate(
    value = ifelse(variable == "predicted_and_covid" & year != 2020, NA, value),
    variable = case_when(
      variable == "total_death_rate" ~ "Observed death rate",
      variable == "fitted_death_rate" ~ "Expected death rate",
      variable == "predicted_and_covid" ~ "Expected + COVID-19 death rate"
    ),
    variable = factor(variable,
      levels = c("Observed death rate", "Expected death rate", "Expected + COVID-19 death rate")
    )
  )

plot_ts_cd <- function(region_bea_name = NA) {
  ggplot(
    plot_df_long %>% filter(region_bea == region_bea_name),
    aes(x = year, y = value, color = variable)
  ) +
    geom_line() +
    geom_point(aes(shape = variable), size = 1.5) +
    geom_ribbon(
      aes(ymin = pred_death_rate_lwr_ci, ymax = pred_death_rate_upr_ci),
      alpha = 0.3, fill = blue_palette[1], linetype = 0
    ) +
    scale_shape_manual(
      values = c(16, 17, 8)
    ) +
    scale_color_manual(values = c("#08519C", "#6BAED6", "#fec44f")) +
    scale_x_continuous(breaks = seq(2011, 2020, 3)) +
    facet_wrap(cs_name ~ ., scales = "free", ncol = 4) +
    plot_theme() +
    labs(
      x = "", y = "",
      title = region_bea_name
    ) +
    theme(
      axis.text = element_text(size = 8),
      strip.text = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5),
      panel.grid.minor = element_line(size = 0.05),
      panel.grid.major = element_line(size = 0.25),
      plot.margin = margin(0, 5, 0, 0, "mm"),
      aspect.ratio = 9 / 16
    )
}

region_bea_name <- c(
  "New England", "Mideast", "Great Lakes", "Plains",
  "Rocky Mountain", "Far West", "Southeast", "Southwest"
)
plot_out <- list()
for (i in 1:10) {
  plot_out[[i]] <- plot_ts_cd(region_bea_name[i])
}

ts_plot1 <- ggarrange(
  plot_out[[1]], plot_out[[2]], plot_out[[3]], plot_out[[4]],
  common.legend = TRUE, legend = "bottom", nrow = 4
)
annotate_figure(ts_plot1,
  left = text_grob("Deaths (Per 100,000 Person-Years)", rot = 90)
)
ggsave(paste0(outpath, "afig_time_series_stacked1.png"), width = 8.5, height = 11)

ts_plot2 <- ggarrange(
  plot_out[[5]], plot_out[[6]], plot_out[[7]], plot_out[[8]],
  common.legend = TRUE, legend = "bottom", nrow = 4
)
annotate_figure(ts_plot2,
  left = text_grob("Deaths (Per 100,000 Person-Years)", rot = 90)
)
ggsave(paste0(outpath, "afig_time_series_stacked2.png"), width = 8.5, height = 11)
