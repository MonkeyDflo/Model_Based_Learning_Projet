# Description calculate_loglik.R file####
# Date : 2020-12-13
# This file regroup functions usefull to calculate loglik
# All functions could be tested with unit tests using testthat package
# For more details see README

#'function CalculateLoglik
#' @param xCate Matrix of categorical variables for input datas
#' @param xConti Matrix of continuous variables for input datas
#' @param KnbClasse number of classes K
#' @param prop Marix of proportions for each classes
#' @param mu Matrix of mus for continuous variables
#' @param sigma Matrix of Variance Covariance matrix for continuous variables
#' @param alpha Probabilities for each categorical variables 
#' @return log likelihood
#' @export
CalculateLoglik = function(xCate, xConti, KnbClasse, prop, mu, sigma, alpha){
  n = nrow(xCate)
  SommeSurIndividus = 0
  for(i in 1:n){
    SommeSurClasses = 0
    for(k in 1:KnbClasse){
      ProduitSurCateg = 1
      for(j in 1:length(alpha[1,])){
        ProduitSurCateg = ProduitSurCateg * alpha[k,j]^xCate[i,j]
      }
      # => une fois qu'on a les produits sur les catégories 
      # on multiplie itérativement par fk(x)
      # mu (ajouter une dim si on veut la stocker en fct du nombre d'itération)
      # sigma (pareil que mu)
      PrdCateConti = ProduitSurCateg * dmvnorm(xConti[i,],mean=mu[k,],sigma=sigma[k,,])
      
      SommeSurClasses = SommeSurClasses + prop[k] * PrdCateetConti
    }
    SommeSurIndividus = SommeSurIndividus + log(SommeSurClasses)
  }
  return(SommeSurIndividus)
}

