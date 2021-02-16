# Description calculate_loglik.R file####
# Date : 2020-12-13
# This file regroup functions usefull to calculate loglik
# All functions could be tested with unit tests using testthat package
# For more details see README

# function MaximisationStep ####
#'function MaximisationStep
#' @param xCate Matrix of categorical variables for input datas
#' @param xConti Matrix of continuous variables for input datas
#' @param KnbClasse number of classes K
#' @param prop Marix of proportions for each classes
#' @param mu Matrix of mus for continuous variables
#' @param sigma Matrix of Variance Covariance matrix for continuous variables
#' @param alpha Probabilities for each categorical variables 
#' @return log likelihood
#' @export
MaximisationStep = function(tk, xBinCate, xConti, KnbClasse, prop, mu, sigma, alpha, ITER){
  # M step ####
  n = nrow(xBinCate)
  # calcul nk
  nk = rep(NA, KnbClasse)
  for(k in 1:KnbClasse){
    nk[k] = sum(tk[,k])
  }
  #actualisation de prop
  # formule = pk() = nk/n 
  for(k in 1:KnbClasse){
    prop[ITER, k] = nk[k]/n
  }
  #actualisation de mus
  for(k in 1:KnbClasse){
    mu[ITER,k] = (1 / nk) * sum( tk[,k] * xConti )
  }
  #actualisation de sigma
  for(k in 1:KnbClasse){
    sigma[ITER, k, , ] = Reduce('+', lapply(1:n, 
                                            function(i) tk[i,k] * (xConti[i,] - mu[ITER,k,]) %*% t(xConti[i,] - mu[ITER,k,]) /nk) ) 
  }
  #actualisation de alpha
  # formule : alpha jhk = 1/nk * somme sur n des tik(xi) * x ijh
  for(k in 1:KnbClasse){
    for(j in 1:ncol(xCate)){
      sumTkxi = sum( tk[,k] * xBinCate )
      alpha[k,j] = ( 1 / nk[k] ) * sumTkxi
    }
  }
  
  return(list(prop = prop, 
              mu = mu, 
              sigma = sigma, 
              alpha = alpha))
}



