# calcul of probabilities

# Conditionnal probability of continous data
conditionalProbCont <- function(continousData, nbClass,propContinousData, mu, sigma){
  require(mclust)
  n <- nrow(continousData)
  
  # Conditional probability tik that represente esperance Q
  tik <- matrix(NA, n, nbClass)
  
  for(k in 1:nbClass)
    tik[,k]<- propContinousData[k]*dmvnorm(continousData, mean=mu[,k], sigma=sigma[k,,])
  tik = tik/rowSums(tik)
  
  return(tik)
}

# Conditionnal probability tik of categorical data
conditionalProbCat <- function

ExpectationStep <-function(categoricalData, nbClass,propcategoricalData){
  # Calcul of tiK and
  tik <- matrix(NA, n, nbClass)
  for(i in 1:nbClass)
    tik[,k] <- propcategoricalData * dmv
}

alpha <- matrix(rnorm(4*5), ncol = 4)

propContinousData = c(0.1, 0.7, 0.2)
mu = matrix(runif(3*4), ncol = 3)
sigma = array(NA, dim=c(3, 4,4))

for(k in 1:3)
  sigma[k,,] = cov(matrix(runif(4*4)))
obs = iris[,-5]
conditionalProbCont(obs, 3, propContinousData=propContinousData, mu=mu, sigma= sigma)
