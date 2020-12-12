# Function initialize

initializeModel <- function(continuousData, categoricalData, nbClass, ITERMAX) {
  require(bayess)
  
  if(is.factor(categoricalData)){
    print(paste("Error categoricalData is a factor vector"))
    return(NULL)
  }
  # data information
  n <- nrow(continuousData);  p <- ncol(continuousData)
  
  #[Object creation] of gaussian model
  propContinousData <- matrix(NA, ITERMAX+1, nbClass)
  mu <- array(NA, dim = c(ITERMAX+1, nbClass, p))
  sigma <- array(NA, dim = c(ITERMAX+1, nbClass, p, p))
  
  # Creation of object that contain alpha an proportion in 
  # multinomial model
  numberOfCategory <- NCOL(categoricalData)
  propCategoricalData <- matrix(NA, ITERMAX+1, nbClass)
  alphaContainer <- list()
  
  for (j in 1: numberOfCategory){
    for( k in 1:nbClass){
      numberOfLevel <- length(unique(categoricalData[,j])) # Variable that contain nber of level
      alpha <- array(NA, dim = c(ITERMAX+1, nbClass, numberOfLevel)) #Array of alpha by iteration, cathegory and level
      alphaContainer[[j]] <- alpha
    }
  }
  
  ## Object initialisation
  # gaussian model
  propContinousData[1,] <- rdirichlet(1, par=rep(1,nbClass))
  mu[1,,] <- as.matrix(continuousData[sample(1:n,nbClass),])
  for (k in 1:nbClass){
    sigma[k,1,,] <- cov(matrix(rnorm(p*p), ncol = p))
  }
  
  # Multinomial
  propCategoricalData[1,] <- rdirichlet(1, par=rep(1,nbClass))
  for(j in 1:numberOfCategory){
    numberOfLevel <- length(unique(categoricalData[,j]))
    for(h in 1:numberOfLevel){
      alphaContainer[[j]][1,,h]<-rdirichlet(n = 1, par = rep(1, nbClass))
    }
  }
  
  return(list(propContinousData = propContinousData,
              mu = mu,
              sigma = sigma,
              propCategoricalData = propCategoricalData,
              alphaContainer = alphaContainer
              ))
}

