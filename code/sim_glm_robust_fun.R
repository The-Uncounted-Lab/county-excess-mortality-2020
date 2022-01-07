
sim_glm_robust <- function(object, n.sims=100, ses = NULL)
{
  object.class <- class(object)[[1]]
  summ <- summary (object, correlation=TRUE, dispersion = object$dispersion)
  coef <- summ$coef[,1:2,drop=FALSE]
  dimnames(coef)[[2]] <- c("coef.est","coef.sd")
  beta.hat <- coef[,1,drop=FALSE]
  if(is.null(ses)){
  sd.beta <- coef[,2,drop=FALSE]
  }
  else{
    sd.beta <- ses
  }
  corr.beta <- summ$corr
  n <- summ$df[1] + summ$df[2]
  k <- summ$df[1]
  V.beta <- corr.beta * array(sd.beta,c(k,k)) * t(array(sd.beta,c(k,k)))
  #beta <- array (NA, c(n.sims,k))
  #    dimnames(beta) <- list (NULL, dimnames(beta.hat)[[1]])
  #    for (s in 1:n.sims){
  #      beta[s,] <- MASS::mvrnorm (1, beta.hat, V.beta)
  #    }
  beta <- MASS::mvrnorm (n.sims, beta.hat, V.beta)
  # Added by Masanao
  beta2 <- array (0, c(n.sims,length(coefficients(object))))
  dimnames(beta2) <- list (NULL, names(coefficients(object)))
  beta2[,dimnames(beta2)[[2]]%in%dimnames(beta)[[2]]] <- beta
  # Added by Masanao
  sigma <- rep (sqrt(summ$dispersion), n.sims)
  
  ans <- new("sim",
             coef = beta2,
             sigma = sigma)
  return(ans)
}