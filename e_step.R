# calcul of probabilities

# Conditionnal probability of continous data####
#' conditionalProbCont
#' Takes continous observation, and return a conditional pobablilie based in the
#' completed likehood esperance.
#' @param continousData
#' @param nbClass
#' @param prop
#' @param mu
#' @param sigma
#' @return tik matrix
conditionalProbCont <- function(continousData, nbClass,prop, mu, sigma){
  require(mclust)
  n <- nrow(continousData)
  
  # Conditional probability tik that represente esperance Q
  tik <- matrix(NA, n, nbClass)
  
  for(k in 1:nbClass)
    tik[,k]<- prop[k]*dmvnorm(continousData, mean=mu[,k], sigma=sigma[k,,])
  tik <- tik/rowSums(tik)
  
  return(tik)
}

# Conditionnal probability tik of categorical data####
#' conditionalProbCat
#' Takes categorical observations and return a conditional pobablilies based in the
#' completed likehood esperance and multinomial distribution.
#' @param categoricalData
#' @param nbClass
#' @param prop
#' @param alpha
#' @return tik matrix
conditionalProbCat <- function(categoricalData, nbClass, prop, alpha){
  
  tik <- matrix(NA, nrow(categoricalData), 3)
  for (k in 1:nbClass){
    for(i in 1:nrow(categoricalData)){
      tmp <- sapply(alpha[k,], FUN = '^', categoricalData[i,])
      tik[i,k] <- prop[k]*prod(diag(tmp))
    }
  }
  tik <- tik/rowSums(tik)
  return(tik)
}

# Expectation step function 
# ExpectationStep ####
#' ExpectationStep
#'  This function takes both continous and categorical observations and performs
#'  the expectation step of EM-algorithm.
#'  @param continousData
#'  @param categoricalData
#'  @param nbClass
#'  @param prop
#'  @param mu
#'  @param sigma
#'  @param alpha
#'  @return list of tik matrix of continuous and categorical observation
ExpectationStep <-function(continousData, categoricalData, nbClass, prop, mu, sigma, alpha){
  continousData <- continousData
  categoricalData <- categoricalData
  nbClass <- nbClass
  prop <- prop
  mu <- mu
  alpha <- alpha
  
  tikContinousData <- conditionalProbCont(continousData, nbClass, prop, mu, sigma)
  tikCategoricalData <- conditionalProbCat(categoricalData, nbClass, prop, alpha)
  
  return(list(tikContinousData = tikContinousData,
              tikCategoricalData = tikCategoricalData))
}

