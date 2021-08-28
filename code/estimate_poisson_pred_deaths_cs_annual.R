#-------------------------------------------------------------------------
#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
library(sandwich)


main_file_path <- "//serv571c/research2/Ackley_571c/covid_main_folder/county_set_update"

ref_tables <- paste0(main_file_path,'/raw_data')
libin <- paste0(main_file_path,'/processed_data')
libout <- paste0(main_file_path,'/final_data')

#-------------------------------------------------------------------------
#Utility Functions and External Macors
any_in <- function(egg,nest){ifelse(sum(egg %in% nest) > 0,1,0)}

#Misc Functions
source(paste0(main_file_path,'/macros/ackley_funs.R'))

#This is a modification of the add_pi function in ciTools which uses robust standard errors
#See https://cran.r-project.org/web/packages/ciTools/vignettes/ciTools-glm-vignette.html
source(paste0(main_file_path,'/macros/sim_glm_robust_fun.R')) 


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Chunk 0: Load input data and conduct minor edits

#-------------------------------------------------------------------------
#Import analysis data
setwd(libin)

dat <- read.csv(file= 'county_set_analysis_data_2011_2019_W2020_wash_6_3.csv', stringsAsFactors = FALSE)
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

base_year <- 1998
dat_edit <- dat %>% filter(year >= 2011, !is.na(cs_code)) %>%
  mutate(time = year - base_year, #Normalize time to 1999=1
         time_orig_vals = time)
dat_edit <- dat_edit %>% arrange(cs_code,year)
dat_est <- dat_edit %>% filter(year < 2020) 
dat_2020 <- dat_edit %>% filter(year == 2020)



fit <- glm(total_deaths ~ offset(log(death_offset)) + death_rate_lag1 + time +
             time*factor(cs_code) , family = quasipoisson(link = "log") ,data = dat_est)


if(1==1){
  setwd(libin)
  save(fit, file = 'estimated_poisson_glm_parameters.rda')
}

#Compute cluster-robust errors
vmat_rob <- vcovCL(fit,cluster = ~ cs_code)
clust_ses <- sqrt(diag(vmat_rob))

if(1==1){
  save(clust_ses, file = 'estimated_poisson_glm_ses.rda')
}
#Add fitted values for all years and predicted values for 2020
dat_est <- dat_est %>%
  mutate(fitted_deaths_all_yrs = exp(predict.glm(fit, dat_est)),
         fitted_death_rate_all_yrs = fitted_deaths_all_yrs/death_offset)

dat_2020 <- dat_2020 %>% 
  mutate(fitted_deaths_2020 = exp(predict.glm(fit, dat_2020)),
         fitted_death_rate_2020 = fitted_deaths_2020/death_offset)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Chunk 2: Estimate prediction 95% interval for all years 
#-------------------------------------------------------------------------
library(arm)
#Set parameters
set.seed(79)
mod_mat <- model.matrix(fit)
alpha <- .05
nsims <- 1000
npreds <- nrow(dat_est)
overdisp <- summary(fit)$dispersion
sim_response_mat <- matrix(NA, ncol = nsims, nrow = npreds)

#Sample from coef distributions
#This is a modification of the add_pi function in ciTools which uses robust standard errors
#See https://cran.r-project.org/web/packages/ciTools/vignettes/ciTools-glm-vignette.html for complete details
sim_coefs <- sim_glm_robust(fit, n.sims = nsims, ses = clust_ses)
#save(sim_coefs, file = 'estimated_poisson_glm_simulated_coefs.rda')


for(i in 1:nsims){
  #Fitted value with new coef draw
  yhat <- dat_est$death_offset * exp (mod_mat %*% sim_coefs@coef[i,])
  
  disp_parm <- yhat / (overdisp - 1) #Set new dispersion parameter
  
  #Draw new death count and fill in matrix. Each row is now a sample of size nsims for each county-year. Each col is a draw.
  sim_response_mat[,i] <- rnegbin(n = npreds,
                                  mu = yhat,
                                  theta = disp_parm)
}



#Gather statistics from simulated distributions
sds <- sqrt(apply(sim_response_mat,1,var))
lwr <- apply(sim_response_mat, 1, FUN = quantile, probs = alpha/2)
upr <- apply(sim_response_mat, 1, FUN = quantile, probs = 1 - alpha / 2)

#Add the computed predictions intervals to main table
dat_est <- dat_est %>% 
  mutate(pred_deaths_lwr_ci = lwr,
         pred_deaths_upr_ci = upr,
         pred_death_rate_lwr_ci = lwr/death_offset,
         pred_death_rate_upr_ci = upr/death_offset,
         pred_death_std_err = sds,
         pred_death_rate_std_err = sds/death_offset
         
  )


#-------------------------------------------------------------------------

#Produce plot to make sure eveything looks fine
check_dat <- dat_est %>% filter(cs_code == '01CS031')

ggplot(check_dat, aes(x = year, y = total_deaths)) +
  ggtitle("Quasipoisson Regression", subtitle = "Model fit (black line), with Prediction intervals (gray), Confidence intervals (dark gray)") +
  geom_point(size = 2) +
  geom_line(aes(x = year, y = fitted_deaths_all_yrs), size = 1.2) +
  geom_ribbon(aes(ymin = pred_deaths_lwr_ci , ymax = pred_deaths_upr_ci), alpha = 0.2)
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Estimate prediction interval for 2020. This repeats everything from prior chunk just using 2020 data
#Need this just to get model matrix for 2020
dat_2020 <- semi_join(dat_2020,dat_est, by = 'cs_code')
dat_est2 <- dat_est %>% filter(time == 19) %>% bind_rows(dat_2020)


fit2 <- glm(total_deaths ~ offset(log(death_offset)) + death_rate_lag1 +
              time +
              time*factor(cs_code) , family = quasipoisson(link = "log") ,data = dat_est2)


mod_mat <- model.matrix(fit2)
mod_mat <- mod_mat[which(mod_mat[,'time'] %in% c(22)),]
npreds <- nrow(dat_2020)
sim_response_mat <- matrix(NA, ncol = nsims, nrow = npreds)


for(i in 1:nsims){
  #Fitted value with new coef draw
  yhat <- dat_2020$death_offset* exp (mod_mat %*% sim_coefs@coef[i,])
  
  disp_parm <- yhat / (overdisp - 1) #Set new dispersion parameter
  
  #Draw new death count and fill in matrix. Each row is now a sample of size nsims for each county-year. Each col is a draw.
  sim_response_mat[,i] <- rnegbin(n = npreds,
                                  mu = yhat,
                                  theta = disp_parm)
}


if(1==1){
  boot_samp_out <- bind_cols(as.data.frame(sim_response_mat),dat_2020[,c('cs_code','death_rate','death_offset')]) %>% 
    rename(all_cause_death_rate_2020 = death_rate, death_offset_2020 = death_offset)
  saveRDS(boot_samp_out, file = 'full_bootstrap_sample_2020.rds')
}

sds <- sqrt(apply(sim_response_mat,1,var))
lwr <- apply(sim_response_mat, 1, FUN = quantile, probs = alpha/2)
upr <- apply(sim_response_mat, 1, FUN = quantile, probs = 1 - alpha / 2)

dat_2020 <- dat_2020 %>% 
  mutate(pred_deaths_lwr_ci_2020 = lwr,
         pred_deaths_upr_ci_2020 = upr,
         pred_death_rate_lwr_ci_2020 = lwr/death_offset,
         pred_death_rate_upr_ci_2020 = upr/death_offset,
         pred_death_std_err = sds,
         pred_death_rate_std_err = sds/death_offset
  )

#NOTE MUST UNLOAD THESE LIBRARIES OR CODE BELOW MAY CRASH. THERE IS A CONFLICT WITH THE USE OF SELECT IN MASS PACKAGE AND DPLYR!
#SOMETIMES THIS UNLOAD FAILS IN A CONTINUOUS RUN OF THIS FILE
unloadNamespace("arm")
unloadNamespace("lme4")
unloadNamespace("MASS")
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Chunk 3: Construct Final Output Data for 2020
#-------------------------------------------------------------------------


dat_2020_out <- dat_2020 %>% 
  rename(
    covid_deaths_2020 = covid_deaths,
    death_offset_2020 = death_offset,
    all_cause_deaths_2020 = total_deaths,
    all_cause_death_rate_2020 = death_rate
  ) %>% 
  mutate(
    covid_death_rate_2020 = covid_deaths_2020/death_offset_2020,
    excess_deaths_2020 = all_cause_deaths_2020 - fitted_deaths_2020,
    excess_assigned_covid_deaths = covid_deaths_2020,
    excess_unassigned_covid_deaths = excess_deaths_2020 - covid_deaths_2020,
    prop_deaths_unassigned = excess_unassigned_covid_deaths/excess_deaths_2020,
    prop_deaths_unassigned = ifelse(prop_deaths_unassigned < 0 | excess_unassigned_covid_deaths < 0,NA, prop_deaths_unassigned),
    excess_death_rate_2020 = all_cause_death_rate_2020 - fitted_death_rate_2020,
    excess_assigned_covid_death_rate = covid_death_rate_2020,
    excess_unassigned_covid_death_rate = excess_death_rate_2020 - covid_death_rate_2020,
    prop_death_rate_unassigned = excess_unassigned_covid_death_rate/excess_death_rate_2020,
    prop_death_rate_unassigned = ifelse(prop_death_rate_unassigned < 0 | excess_unassigned_covid_death_rate < 0,NA, prop_death_rate_unassigned)
  )


setwd(libout)
write.csv(dat_2020_out, file = 'fitted_and_actual_deaths_county_sets_2020_W2020_wash_6_3.csv')


dat_newrows_2020 <- dat_2020 %>% 
  rename(
    fitted_deaths_all_yrs = fitted_deaths_2020,
    fitted_death_rate_all_yrs = fitted_death_rate_2020,
    pred_deaths_upr_ci = pred_deaths_upr_ci_2020,
    pred_deaths_lwr_ci = pred_deaths_lwr_ci_2020,
    pred_death_rate_upr_ci = pred_death_rate_upr_ci_2020,
    pred_death_rate_lwr_ci = pred_death_rate_lwr_ci_2020)

dat_pan <- dat_est %>%
  bind_rows(.,dat_newrows_2020) %>% arrange(cs_code, time)


dat_pan_out <- dat_pan %>% 
  rename(
    total_death_rate = death_rate,
    fitted_deaths = fitted_deaths_all_yrs,
    fitted_death_rate = fitted_death_rate_all_yrs,
    population = cens_pop_est) %>%
  mutate(death_offset = (population)/1000)


#Output primary panel data from 2011-2020 used to make tables and figures
setwd(libout)
write.csv(dat_pan_out, file = 'fitted_and_actual_deaths_county_sets_2011_2020_W2020_wash_6_3.csv', row.names = FALSE)



