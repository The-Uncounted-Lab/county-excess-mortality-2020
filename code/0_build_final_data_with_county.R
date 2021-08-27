## convert county-set level final data to county level
## match each county to metropolitan status and BEA region

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("prelim_plot.R")

## load data ----
dat_xc <- read_csv(paste0(datapath, "fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv"))

## match county sets to county
county_sets <- read_csv("../raw_data/county_set_xwalk.csv")
state_coord <- usmap::us_map()
county_sets <- left_join(
  county_sets,
  state_coord %>% transmute(state_fips = as.numeric(fips), state = abbr) %>% distinct(),
  by = "state_fips"
)
dat_xc <- left_join(
  dat_xc, county_sets,
  by = c("state", "cs_name")
)

## exclude counties that do not have data available
county_with_est <- read_csv("../raw_data/county_county_set_data_2020_wash_6_3.csv")
county_with_est <- county_with_est %>%
  transmute(county_code, keep = TRUE)
dat_xc <- left_join(
  dat_xc, county_with_est,
  by = "county_code"
)
dat_xc <- dat_xc %>% filter(keep == TRUE)

## metropolitan status
metro <- read_csv("../raw_data/FIPSmetroregion4cat.csv")
metro <- metro %>%
  transmute(
    county_code = fips,
    state, metroname
  )
dat_xc <- left_join(
  dat_xc, metro,
  by = c("county_code", "state")
)

## BEA regions
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

## fill out missing metro
dat_xc <- dat_xc %>%
  mutate(
    metroname = case_when(
      county_code %in% c(2068, 2105, 2198, 2230, 2275, 2282, 
                         2013, 2016, 2164, 2270, 46113, 2130,
                         2188, 2290, 4012, 30067) ~ "Nonmetro",
      county_code %in% c(8001, 8014) ~ "Lg fringe metro",
      county_code %in% c(8013, 8123, 51515) ~ "Md/Sm metro",
      TRUE ~ metroname
    )
  )

saveRDS(dat_xc, file = "../final_data/fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3_countyrow.rds")
