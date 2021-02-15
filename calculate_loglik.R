# Description calculate_loglik.R file####
# Date : 2020-12-13
# This file regroup functions usefull to calculate loglik
# All functions could be tested with unit tests using testthat package
# For more details see README

# function CalculateLoglik ####
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
CalculateLoglik = function(xCate, xConti, KnbClasse, prop, mu, sigma, alpha, ITER){
  n = nrow(xCate)
  SommeSurIndividus = 0
  for(i in 1:n){
    SommeSurClasses = 0
    for(k in 1:KnbClasse){
      
      ProduitSurCateg = 1
      for(j in 1:length(alpha[1,])){
        
        # for(j in 1:numberOfCategory){
        #   numberOfLevel <- length(unique(categoricalData[,j]))
        #   for(h in 1:numberOfLevel){
        #     alphaContainer[[j]][1,,h]<-rdirichlet(n = 1, par = rep(1, nbClass))
        #   }
        # }
        ProduitSurCateg = ProduitSurCateg * alpha[k,j]^xCate[i,j]
      }
      # => une fois qu'on a les produits sur les catégories 
      # on multiplie itérativement par fk(x)
      # mu (ajouter une dim si on veut la stocker en fct du nombre d'itération)
      # sigma (pareil que mu)
      print(paste('dmvnorm'))
      currX = xConti[i,]
      currMu = mu[ITER,k,]
      currSigma = sigma[ITER,k,,]
      fkConti= dmvnorm(currX,currMu,currSigma) 
      PrdCateConti = ProduitSurCateg * fkConti
      
      SommeSurClasses = SommeSurClasses + prop[ITER,k] * PrdCateConti
    }
    SommeSurIndividus = SommeSurIndividus + log(SommeSurClasses)
  }
  return(SommeSurIndividus)
}



# tests ####
# split ... 
source(file = "organize_dataset.R")
source(file = "initialisation.R")
res = splitDatasetIntoCatAndConti(iris)
cat = res$categoricalMat
resCate = binarizeCateMatrix(cat)
# initialise 
init = initializeModel(res$continuousMat, resCate, 3, 10)

# test loglik
loglik = CalculateLoglik(resCate, res$continuousMat, 3, init$prop, init$mu, init$sigma, init$alpha, ITER=1)
print(loglik)
