# calcul of probabilities

# Conditionnal probability of continous data
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

# Conditionnal probability tik of categorical data
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

alpha <- matrix(rnorm(4*5), ncol = 4)

prop = c(0.1, 0.7, 0.2)
mu = matrix(runif(3*4), ncol = 3)
sigma = array(NA, dim=c(3, 4,4))

for(k in 1:3)
  sigma[k,,] = cov(matrix(runif(4*4)))
obs = iris[,-5]
conditionalProbCont(obs, 3, prop=prop, mu=mu, sigma= sigma)
