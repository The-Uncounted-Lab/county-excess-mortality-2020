

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#OLS bootstrap function

ols_with_boot_ses <- function(dat_with_covs,boot_samples ,model_formula, n_reps = 1000){
  
  
  ols_base <-  summary(lm(as.formula(paste0('excess_death_rate_2020 ',model_formula)) , data= dat_with_covs))
  
  for(i in 1:n_reps){
    
    boot_i <- boot_samples %>% 
      select(county_code, all_cause_death_rate_2020, death_offset_2020)  %>% 
      mutate(pred_deaths_boot_i = boot_samples[,i],
             pred_death_rate_boot_i = pred_deaths_boot_i/death_offset_2020,
             excess_death_rate_boot_i =  all_cause_death_rate_2020 - pred_death_rate_boot_i) %>%
      select(county_code,excess_death_rate_boot_i)
    
    dat_with_boot_i <- dat_with_covs %>% left_join(.,boot_i, by = 'county_code')
    
    ols_sum <- summary(lm(as.formula(paste0('excess_death_rate_boot_i ',model_formula)) , data= dat_with_boot_i))
    
    coef_df <- data.frame(coefs = ols_sum$coefficients[,1])
    
    colnames(coef_df) <- paste0('trial_',i)
    
    if(i == 1){
      coef_ests <- coef_df
    }
    if(i > 1){
      coef_ests <- cbind(coef_ests,coef_df)
    }
  }
  sam_sd <- function(x){
    n <-length(x)
    xbar = mean(x)
    se <- sqrt(sum((x - xbar)^2)/(n-1))
    return(se)
    
  }
  
  boot_std_errors <- apply(coef_ests,1, sam_sd)
  
  #Replace ordinary std errors with the bootstrap ones
  ols_base$coefficients[,2] <-  boot_std_errors
  ols_base$coefficients[,3] <- ols_base$coefficients[,1]/ols_base$coefficients[,2]
  return(ols_base)
}


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Coefficient vplot function
coef_vplot <- function(dta,x_label = "",
                       y_label = 'Coefficient Estimate',
                       lab_size = 12, yrange = NULL, ylimscale = 2){
  
  if(is.null(yrange)){
    ulim <- max(abs(min(dta$coef)),abs(max(dta$coef)))
    yrange = c(-ylimscale*ulim, ylimscale*ulim)
  }
  
  ggplot(data = dta, aes(x = quintile, y = coef))  + 
    geom_point(size = 4) +
    geom_errorbar(aes(ymin=coef-1.96*ses, ymax=coef+1.96*ses), width=.25,
                  size = .75) +
    geom_hline(yintercept=0, linetype='dashed', col = 'red', size=.75)+
    ylim(range(yrange)) +
    labs(x = x_label, y = y_label) +
    coord_flip() + 
    theme(axis.title.y = element_text(size = lab_size),
          axis.title.x = element_text(size = lab_size)
    ) 
}



#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

plot_scatter <- function(dta,var, xname, fit_method = lm, non_neg = FALSE){
  y_top <- quantile(dta %>% select({{var}}) %>% pull(),.9, na.rm=TRUE)
  d <- dta %>%
    filter(!is.na({{var}})) %>% 
    filter({{var}} < y_top)
  
  if(non_neg == TRUE){
    d <- d %>% filter({{var}} >=0)
  }
  
  s <- ggplot(d, aes(x = {{var}},y = excess_death_rate_2020)) + geom_point() +
    labs(x = xname, y ='Excess Death Rate') +
    geom_smooth(method = fit_method) 
  
  return(s)
}

