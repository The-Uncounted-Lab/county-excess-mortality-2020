## make appendix Fig 1: excess death rate and covid-to-excess ratio by BEA region and metro

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

## exclude counties with negative excess mortality
dat_xc_countyset <- dat_xc_countyset %>%
  filter(excess_deaths_2020 > 0)

vdat <- dat_xc_countyset %>%
  group_by(region_bea, metroname_mode) %>%
  summarise(across(
    c(
      "covid_deaths_2020", "fitted_deaths_2020",
      "all_cause_deaths_2020", "cens_pop_est"
    ),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  ungroup() %>%
  mutate(
    offset = cens_pop_est / 100000,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    excess_death_rate = excess_deaths_2020 / offset,
    covid_excess_ratio = covid_deaths_2020 / excess_deaths_2020,
    region_bea = factor(
      region_bea,
      levels = c(
        "Mideast", "Great Lakes", "New England", "Plains",
        "Southeast", "Southwest", "Far West", "Rocky Mountain"
      )
    )
  )

vdat_long <- vdat %>%
  transmute(region_bea, metroname_mode, excess_death_rate, covid_excess_ratio = covid_excess_ratio * 100) %>%
  gather(var, val, -region_bea, -metroname_mode)
vdat_long <- vdat_long %>%
  mutate(
    mean = case_when(
      var == "excess_death_rate" ~ 135,
      var == "covid_excess_ratio" ~ 86.7
    ),
    label = case_when(
      var == "excess_death_rate" ~ "Weighted mean = 135",
      var == "covid_excess_ratio" ~ "Weighted mean = 87"
    ),
    ypos = case_when(
      var == "excess_death_rate" ~ 194,
      var == "covid_excess_ratio" ~ 113
    ),
    var = case_when(
      var == "excess_death_rate" ~ "Excess deaths (Per 100,000 Person-Years)",
      var == "covid_excess_ratio" ~ "Ratio of COVID-19 deaths to excess deaths (%)"
    ),
    var = factor(
      var,
      levels = c(
        "Excess deaths (Per 100,000 Person-Years)",
        "Ratio of COVID-19 deaths to excess deaths (%)"
      )
    )
  )
vdat_long <- vdat_long %>%
  group_by(var) %>%
  mutate(
    n = row_number(),
    label = ifelse(n == 1, label, NA)
  )

ggplot(vdat_long) +
  geom_bar(aes(
    x = region_bea, y = val,
    fill = metroname_mode, group = metroname_mode
  ),
  stat = "identity",
  position = "dodge"
  ) +
  geom_text(aes(
    x = region_bea, y = val,
    label = as.integer(val), color = metroname_mode
  ),
  position = position_dodge(width = 0.9),
  hjust = 1.2,
  size = 2.7
  ) +
  labs(
    title = "", x = "BEA Region",
    y = ""
  ) +
  facet_wrap(~var, scales = "free_x") +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = c("white", "white", "white", "black")) +
  scale_fill_manual(
    name = "Metro:",
    labels = c("Lg central metro", "Lg fringe metro", "Md/Sm metro", "Nonmetro"),
    values = rev(blue_palette4)
  ) +
  geom_hline(aes(yintercept = mean), linetype = "dashed") +
  geom_text(
    size = 3,
    x = "Far West",
    aes(label = label, y = ypos)
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(size = 11)
  ) +
  guides(color = "none")
ggsave(filename = paste0(outpath, "afig1_excess_regionmetro_bar.png"), width = 8, height = 7)
