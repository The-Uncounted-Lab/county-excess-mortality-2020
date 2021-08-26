## make Fig 1: distribution of death rate

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

# load data
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))
dat_pan <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2011_2020_W2020_wash_6_3.csv"))

death_rate <- dat_pan %>%
  transmute(
    state, cs_name, year,
    total_death_rate = total_death_rate * 100,
    fitted_death_rate = fitted_death_rate * 100,
    excess_death_rate = total_death_rate - fitted_death_rate,
    excess_death_rate = ifelse(abs(excess_death_rate) > 500, NA, excess_death_rate),
    total_death_rate = ifelse(total_death_rate > 2500, NA, total_death_rate)
  ) %>%
  select(-fitted_death_rate) %>%
  gather(variable, value, -state, -cs_name, -year) %>%
  mutate(
    year = factor(year),
    variable = case_when(
      variable == "total_death_rate" ~ "All-Cause Death Rate (Per 100,000 Person-Years)",
      variable == "excess_death_rate" ~ "Excess Death Rate (Per 100,000 Person-Years)"
    )
  )
noncovid_excess_death_rate_2020 <- dat_xc %>%
  transmute(
    state, cs_name,
    year = "2020",
    variable = "Excess Death Rate Excluding COVID-19 (Per 100,000 Person-Years)",
    value = excess_unassigned_covid_death_rate * 100
  )
noncovid_excess_death_rate_1119 <- death_rate %>%
  filter(year != "2020", variable == "Excess Death Rate (Per 100,000 Person-Years)") %>%
  mutate(variable = "Excess Death Rate Excluding COVID-19 (Per 100,000 Person-Years)")

death_rate <- bind_rows(
  death_rate, noncovid_excess_death_rate_2020, noncovid_excess_death_rate_1119
) %>%
  mutate(
    year = factor(year,
      levels = c(
        "2011", "2012", "2013", "2014", "2015",
        "2016", "2017", "2018", "2019", "2020"
      )
    ),
    value = ifelse(
      variable == "Excess Death Rate Excluding COVID-19 (Per 100,000 Person-Years)" &
        abs(value) > 300, NA, value
    )
  )

mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(20)
ggplot(
  death_rate,
  aes(x = value, color = year, group = year)
) +
  geom_density() +
  facet_wrap(~variable, scales = "free", ncol = 1) +
  scale_color_manual(values = c(mycolors[5:13], "black")) +
  scale_size_manual(values = c(rep(0.5, 9), 1), guide = "none") +
  labs(
    y = "Density", x = "",
    color = "Year", fill = "Year"
  ) +
  plot_theme() +
  theme(strip.text = element_text(size = 11)) +
  guides(colour = guide_legend(nrow = 1))
ggsave(paste0(outpath, "fig1_hist_2011_2020.png"), width = 8, height = 9)
