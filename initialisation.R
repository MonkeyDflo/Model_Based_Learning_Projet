# Function initialize

initializeModel <- function(continuousData, categoricalData,  nbOfClass, ITERMAX) {
  # data information
  n <- nrow(continuousData);  p <- ncol(continuousData)
  
  #[Object creation] of gaussian model
  propContinousData <- matrix(NA, ITERMAX+1, nbOfClass)
  mu <- array(NA, dim = c(ITERMAX+1, nbOfClass, p))
  sigma <- array(NA, dim = c(ITERMAX+1, nbOfClass, p, p))
  
  # Creation of object that contain alpha an proportion in 
  # multinomial model
  numberOfCategory <- ncol(categoricalData)
  propCategoricalData <- matrix(NA, ITERMAX+1, nbOfClass)
  alphaContainer <- list()
  
  for (h in 1: numberOfCategory){
    for( k in 1:nbOfClass){
      numberOfLevel <- length(unique(categoricalData[,h])) # Variable that contain nber of level
      alpha <- array(NA, dim = c(ITERMAX+1, nbOfClass, numberOfLevel)) #Array of alpha by iteration, cathegory and level
      alphaContainer[[h]] <- alpha
    }
  }
  
  ## Object initialisation
  # gaussian model
  propContinousData[1,] <- rdirichlet(1, par=rep(1,nbOfClass))
  mu[1,,] <- as.matrix(continuousData[sample(1:n,nbOfClass),])
  for (k in 1:nbOfClass){
    sigma[k,1,,] <- cov(matrix(rnorm(p*p), ncol = p))
  }
  
  # Multinomial
  propCategoricalData[1,] <- rdirichlet(1, par=rep(1,nbOfClass))
  for(h in 1:numberOfCategory){
    numberOfLevel <- length(unique(categoricalData[,h]))
    for(j in 1:numberOfLevel){
      alphaContainer[[h]][1,,j]<-rdirichlet(n = 1, par = rep(1, nbOfClass))
    }
  }
}

