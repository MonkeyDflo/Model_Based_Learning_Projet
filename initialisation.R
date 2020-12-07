# Function initialize

initializeModel <- function(continuousData, categoricalData,  numberOfClass, ITERMAX) {
  # data information
  n <- nrow(continuousData);  p <- ncol(continuousData)
  
  #[Object creation] of gaussian model
  proportionContinousData <- matrix(NA, ITERMAX+1, numberOfClass)
  mu <- array(NA, dim = c(ITERMAX+1, numberOfClass, p))
  sigma <- array(NA, dim = c(ITERMAX+1, numberOfClass, p, p))
  
  # Creation of object that contain alpha an proportion in 
  # multinomial model
  numberOfCategory <- ncol(categoricalData)
  proportionCategoricalData <- matrix(NA, ITERMAX+1, numberOfClass)
  alphaContainer <- list()
  
  for (h in 1: numberOfCategory){
    for( k in 1:numberOfClass){
      numberOfLevel <- length(unique(categoricalData[,h])) # Variable that contain nber of level
      alpha <- array(NA, dim = c(ITERMAX+1, numberOfClass, numberOfLevel)) #Array of alpha by iteration, cathegory and level
      alphaContainer[[h]] <- alpha
    }
  }
  
  ## Object initialisation
  # gaussian model
  proportionContinousData[1,] <- rdirichlet(1, par=rep(1,numberOfClass))
  mu[1,,] <- as.matrix(obs[sample(1:n,numberOfClass),])
  for (k in 1:numberOfClass){
    sigma[k,1,,] <- cov(matrix(rnorm(p*p), ncol = p))
  }
  
  # Multinomial
  proportionCategoricalData[1,] <- rdirichlet(1, par=rep(1,numberOfClass))
  for(h in 1:numberOfCategory){
    numberOfLevel <- length(unique(categoricalData[,h]))
    for(j in 1:numberOfLevel){
      alphaContainer[[h]][1,,j]<-rdirichlet(n = 1, par = rep(1, numberOfClass))
    }
  }
  
}
