#Common functions I Use


#################################################################################################################
#################################################################################################################
#Data Wrangling
#################################################################################################################


#################################################################################################################
Mode <- function(x, na.rm = FALSE) {
  
  
  if(na.rm == TRUE){
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
  } else{ ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
}

vmode <- Vectorize(Mode,'x')
#################################################################################################################

#################################################################################################################
`%notin%` = function(x,y){ !(x %in% y)}
#################################################################################################################


#################################################################################################################
uni_replace <- function(x, value){
  #nas
  z <- ifelse(is.na(x), value, x)
  
  #nans
  z <- ifelse(is.nan(x), value, z)
  
  #infiniy
  z <- ifelse(is.infinite(x), value, z)
  
  return(z)
}
#################################################################################################################


#################################################################################################################
meanfilt <- function(x, nonneg = FALSE, to_numeric = FALSE, censor = FALSE){
  z <- x
  if(to_numeric == TRUE){z <- as.numeric(z)}
  
  z <- z[!is.na(z)]
  z <- z[!is.nan(z)]
  z <- z[is.finite(z)]
  if(nonneg == TRUE){z <- z[z >= 0]}
  if(censor ==TRUE){z <- z[z > 0]}
  z <- as.numeric(mean(z))
  return(z)
  }
#################################################################################################################
medfilt <- function(x, nonneg = FALSE, to_numeric = FALSE){
  z <- x
  if(to_numeric == TRUE){z <- as.numeric(z)}
  
  z <- z[!is.na(z)]
  if(nonneg == TRUE){z <- z[z >= 0]}
  z <- as.numeric(median(z))
  return(z)
  
}
#####################################################################################################
#################################################################################################################
#Tables
transpose_table <- function(dat, cnames = NULL, rnames = NULL){
  dtab <- as.data.frame(dat)
  dtab1 <- matrix(ncol = nrow(dtab), nrow = ncol(dtab) - 1)
  
  for(j in 1:ncol(dtab1)){
    dtab1[,j] <- as.numeric(dtab[j,2:ncol(dtab)])
  }
  
  if(!is.null(cnames)){colnames(dtab1) <- cnames}
  if(!is.null(rnames)){rownames(dtab1) <- rnames}
  
  dtab1 <- as.data.frame(dtab1)
  return(dtab1)
}



#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#ECONOMETRICS

#################################################################################################################
#################################################################################################################
#Synth add-on
ackley_synth <- function(d_in, x_in, yrs_in, yrs_prior_in, idvar_in, tvar_in, treated_in, depvar_in){ 
  
  idvar_in <- as.name(idvar_in)
  tvar_in <- as.name(tvar_in)
  depvar_in <- as.name(depvar_in)

  
  #First create balanced panel and numeric ids
  d1 <- d_in %>% group_by(!!enquo(idvar_in)) %>% 
    mutate(bal = ifelse(length(intersect(yrs_in,!!enquo(tvar_in))) == length(yrs_in),1,0)) %>%  ungroup() %>% 
    filter(bal == 1, !!enquo(tvar_in) %in% yrs_in) %>% 
    select(-one_of(c('bal'))) 
  
  
  
  d2 <- d1 %>% distinct(!!enquo(idvar_in)) %>% mutate(num = row_number())
  
  d1 <- d1 %>% left_join(.,d2, by = as.character(idvar_in))
  
  
  
  d1 <- as.data.frame(d1)
  
  idvar_in <- as.character(idvar_in)
  treated_in <- as.character(treated_in)
  tvar_in <- as.character(tvar_in)
  depvar_in <- as.character(depvar_in)
  
  tunits <- as.numeric(unique(d1[d1[,idvar_in] == treated_in, 'num']))
  
  ctrl <- unique(d1$num)
  ctrl <- ctrl[which(ctrl %notin% tunits)]
  
  
  
  dataprep.out <- dataprep(foo = d1, predictors = x_in, dependent = depvar_in, unit.variable = 'num' ,
                           time.variable = tvar_in, treatment.identifier = tunits,
                           time.predictors.prior = yrs_prior_in, unit.names.variable = idvar_in, controls.identifier = ctrl,
                           time.optimize.ssr = yrs_prior_in, time.plot = yrs_in)
  
  synth.out <- synth(dataprep.out)
  
  
  y0 <- dataprep.out$Y0plot %*% synth.out$solution.w
  y1 <- dataprep.out$Y1plot
  t <- dataprep.out$tag$time.plot
  
  pdat <- as_tibble(as.matrix(cbind(y0,y1,t)))
  colnames(pdat) <- c('synthetic','treated','time')
  
  pdat <- gather(pdat, !!depvar ,key = 'group', 'synthetic', 'treated', -time)
  
  return(pdat)
}
#################################################################################################################
#################################################################################################################

graph_range <- function(dat, width){
  sd <- sqrt(var(dat))
  l <- min(dat) - sd/width
  h <- max(dat) + sd/width
  j <- c(l,h)
  return(j)
}

#################################################################################################################
#################################################################################################################
#Event Study Plot
event_plot <- 
  function (dat,
            y_val,treat_val, time_val, se_val,ylim = NULL, y_lab = NULL, 
            ci = FALSE,zscore = NULL ,treat_lab = NULL,
            time_lab = NULL, title = NULL, lab_size = NULL, title_size = NULL,
            hline = FALSE) 
  {
    
    results <- data.frame(matrix(nrow = nrow(dat), ncol = 4))
    results$ate <- dat[,y_val]
    results$time <- as.factor(dat[, time_val])
    results$ate.se <- dat[, se_val]
    if(is.null(zscore)){zscore = 1.96}
    if(ci == TRUE){ results$ate.se <- results$ate.se*zscore}
    results$treat <- as.factor(dat[,treat_val])
   
  
    if(is.null(ylim)){ylim = c(1.5*min(results$ate), 1.5*max(results$ate))}
    if(is.null(treat_lab)){treat_lab <- 'Treated'}
    if(is.null(time_lab)){time_lab <- 'Time'}
    if(is.null(y_lab)){y_lab <- 'Treatment Effect'}
    if(is.null(title)){title <- ""}
    if(is.null(lab_size)){lab_size <- 14 }
    if(is.null(title_size)){title_size <- 14}
    
    
    mplot <- ggplot(results, aes(x = time, y = ate, color = treat, shape = treat))  + 
        geom_errorbar(aes(ymin=ate-ate.se, ymax=ate+ate.se), width=.1) +
        geom_point(size = 3) + ylim(range(ylim)) + scale_shape_discrete(name = treat_lab ,breaks = c('0','1') ,labels = c('Pre','Post')) +
        scale_color_discrete(name = treat_lab ,breaks = c('0','1') ,labels = c('Pre','Post'), guide = FALSE) + 
        labs(x = time_lab, y = y_lab, title = title) +
        theme_bw() + theme(axis.title.y = element_text(size = lab_size),
                           axis.title.x = element_text(size = lab_size), legend.title = element_text(size = 12),
                           legend.text = element_text(size = 10), title = element_text(size = title_size)) 
     
  
    
    
    return(mplot)
    
  }

