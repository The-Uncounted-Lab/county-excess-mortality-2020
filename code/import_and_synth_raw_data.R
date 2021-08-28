
#-------------------------------------------------------------------------
#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
#-------------------------------------------------------------------------
#Directories

#Set path to replication ket folder
main_file_path <- "..."

rawdat <- paste0(main_file_path,'/raw_data')
procdat <- paste0(main_file_path,'/processed_data')

#Misc Functions
source(paste0(main_file_path,'/macros/ackley_funs.R'))
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
setwd(rawdat)
#Load histoical census pop data and shape into a panel
cens_dat <- read.csv(file = '3_census_counties_2011_2019.csv', stringsAsFactors = FALSE)
colnames(cens_dat) <- tolower(colnames(cens_dat))
cens_dat <- cens_dat %>% mutate(county_code = paste0(state,str_pad(county,3, pad = "0")))

#keep only population estimates
cens_dat <- cens_dat %>% select(county_code, starts_with('popestimate'))

#shape into panel
cens_dat <- cens_dat %>% pivot_longer(cols = starts_with('popestimate'), values_to = 'cens_pop_est', names_to = 'year') %>% 
  mutate(year = as.numeric(substr(year,12,15)),
         county_code = as.numeric(county_code))


#-------------------------------------------------------------------------
#Load 2020 county-level provisional data
#Note this file is exactly: https://data.cdc.gov/NCHS/AH-County-of-Residence-Provisional-COVID-19-Deaths/75vb-d79q
dat_2020 <- read.csv(file ='covid_and_AC_deaths_2020_by_county_rsd_washup_6_3.csv',
                     stringsAsFactors = FALSE )
colnames(dat_2020) <- tolower(colnames(dat_2020))
dat_2020 <- dat_2020 %>% 
  rename(county_code = fips.code,
         county_name = county.of.residence,
          total_deaths = total.deaths,
         covid_deaths = covid.19.deaths)

popdat_2020 <- read.csv(file = 'pop_2020_simplified.csv', stringsAsFactors = FALSE)
popdat_2020 <- popdat_2020 %>% rename(county_code = fipscountycode, cens_pop_est = pop_2020)

dat_2020 <- dat_2020 %>% left_join(.,popdat_2020, by = 'county_code') %>% 
  select(state,county_code, county_name, covid_deaths, total_deaths,
         cens_pop_est) %>% mutate(total_deaths = ifelse(is.na(total_deaths),5,total_deaths),
                                  covid_deaths = ifelse(is.na(covid_deaths),5,covid_deaths),
                                  year = 2020)

#Combine 
raw_panel <- inner_join(cens_dat, dat_2020[,c('county_code','county_name','state')], by = 'county_code') 


#-------------------------------------------------------------------------
#Load yearly mortality files downloaded from from CDC Wonder. These are split into two chunks.
#Note years prior to 2011 may not be necessary, but we'll preserve flexibility
ydat1 <- read.table(file = 'Underlying Cause of Death, 1999-2009.txt', fill = TRUE, header=TRUE,
                    stringsAsFactors = FALSE)
colnames(ydat1) <- tolower(c(colnames(ydat1)[2:ncol(ydat1)],"V"))
last_line1 <- which(ydat1$county == "---")[1]-1
ydat1 <- ydat1[1:last_line1, c(1:8)]

ydat2 <- read.table(file = 'Underlying Cause of Death, 2009-2019.txt', fill = TRUE, header=TRUE,
                    stringsAsFactors = FALSE)
colnames(ydat2) <- tolower(c(colnames(ydat2)[2:ncol(ydat2)],"V"))
last_line2 <- which(ydat2$county == "---")[1]-1
ydat2 <- ydat2[1:last_line2, c(1:8)]
ydat2 <- ydat2 %>% filter(year != 2009)

#Combine into one table
ydat <- bind_rows(ydat1,ydat2) %>% 
  rename(crude_rate = crude,
         age_adj_rate = rate,
         total_deaths = deaths,
         county_code = county.code) %>% 
  select(-year.code) %>% 
  mutate(
    crude_rate = as.numeric(crude_rate),
    age_adj_rate = as.numeric(age_adj_rate),
    age_adj_conv = age_adj_rate/crude_rate) %>% arrange(county_code,year)
remove(ydat1,ydat2)
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#Add census population estimates (rename CDC population estimate for clarity)
all_dat_pan <- raw_panel %>% 
  left_join(.,ydat[,c('county_code','year','total_deaths')], by = c('county_code','year')) %>% 
  bind_rows(.,dat_2020) %>% arrange(county_code, year)

#Compute offsets as population/1000 to get rates per 1000 people and death rates
county_panel_final <- all_dat_pan %>%
  mutate(
    total_deaths = ifelse(is.na(total_deaths),5,total_deaths), #impute 5 for missings
    death_offset = (cens_pop_est)/1000,
    death_rate = total_deaths/death_offset,
    covid_death_rate = covid_deaths/death_offset
  ) %>% 
  group_by(county_code) %>% 
  mutate(total_deaths_lag1 = lag(total_deaths),
         death_rate_lag1 = lag(death_rate),
         pop_lag1 = lag(cens_pop_est)
  )



#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Construct county-sets
setwd(rawdat)
cs_xwalk <- read.csv(file = 'county_set_xwalk.csv', stringsAsFactors = FALSE,
                     blank.lines.skip = TRUE)
cs_xwalk <- cs_xwalk %>% mutate(county_code = paste0(county_code)) 


dat_cs <- county_panel_final %>% 
  mutate(county_code = paste0(county_code)) %>% 
  left_join(.,cs_xwalk[,c('cs_code','cs_name','county_code')], by = 'county_code') %>% 
  filter(!is.na(cs_code))




dat_cs_final <- dat_cs %>% 
  group_by(cs_code,year) %>% 
  summarise(
    across(c('state','cs_name'),  first),
    across(c('total_deaths','cens_pop_est','total_deaths_lag1', 'pop_lag1','covid_deaths'), ~sum(.x, na.rm = TRUE)),
    num_counties = n()
  ) %>% ungroup() %>% 
  mutate(death_offset = (cens_pop_est)/1000,
         death_offset_lag1 = pop_lag1/1000,
         death_rate = total_deaths/death_offset,
         death_rate_lag1 = total_deaths_lag1/death_offset_lag1) 

#Output final data
if(1==0){
  setwd(procdat)
  write.csv(dat_cs_final, file = 'county_set_analysis_data_2011_2019_W2020_wash_6_3.csv', row.names = FALSE)
}



