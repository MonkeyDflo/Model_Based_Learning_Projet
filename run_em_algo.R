# Importation of sources functions
source("organize_dataset.R")
source("initialisation.R")
source("calculate_loglik.R")
source("e_step.R")
source("m_step.R")


# EM algorithm function
#' runEMAlgoritm
#' This function perform the whole EM algorithm
#' @param data
#' @param nbClass
#' @param ITERMAX, 
runEMAlgoritm <- function(data, nbClass, ITERMAX, mode = "random"){
  # Split of data
  # In this step data are split in 2 dataset continous and categorical
  dataset <- splitDatasetIntoCatAndConti(data)
  
  categoricalData <- binarizeCateMatrix(dataset$categoricalMat)
  continousData <- dataset$continuousMat
  # Initialization
  # Creation of objects that will contain parameters of models
  if(mode == "random"){
    ## initialization of random model
    init <- initializeModel(continuousData=continousData, 
                            categoricalData=categoricalData, 
                            nbClass=nbClass, 
                            ITERMAX = ITERMAX)
    prop <- init$prop
    mu <- init$mu
    sigma <- init$sigma
    alpha <- init$alpha
    loglik <- init$loglik
    
    
  } else if(mode == "kmeans"){
    ## initialization of kmeans model
    # TODO: implementer l'initialtion kmeans
  } else {
   stop("The selected mode is invalid")
  }
  
  
  # Beggin of loops
  ITER <- 1
  LIMITE <- 1
  # loglik[1] <- CalculateLoglik(continousData, 
  #                              categoricalData, 
  #                              nbClass,
  #                              prop,
  #                              mu,
  #                              sigma,
  #                              alpha,
  #                              1)
  loglik[1] <- loglikCalul(continousData,
                               categoricalData,
                               nbClass,
                               prop,
                               mu,
                               sigma,
                               alpha,
                               1)
  
  while(ITER <= ITERMAX){
    # Perform Expectaion step
    tik <- ExpectationStep(continousData, 
                           categoricalData, 
                           nbClass, 
                           prop, 
                           mu,
                           sigma,
                           alpha,
                           ITER)
    
    # Perform Maximization step
    # estimation of paramater prop, mu, sigma and alpha
    mStepRes <- MaximisationStep(tik, 
                     categoricalData, 
                     continousData, 
                     nbClass,
                     prop, 
                     mu, 
                     sigma, 
                     alpha, 
                     ITER)
    prop[ITER+1,] <- mStepRes$prop
    mu[ITER+1,,] <- mStepRes$mu
    sigma[ITER+1,,,] <- mStepRes$sigma
    alpha <- mStepRes$alpha
    # prop = mStepRes$prop 
    # mu = mStepRes$mu 
    # sigma = mStepRes$sigma 
    # alpha = mStepRes$alpha
    
    # Calcul of loglik of the next interation
    loglik[ITER+1] <- loglikCalul(continousData, 
                                      categoricalData, 
                                      nbClass,
                                      prop,
                                      mu,
                                      sigma,
                                      alpha,
                                      ITER+1)
    
    
    ITER <- ITER +1
    LIMITE <- loglik[ITER+1]-loglik[ITER]
    # if(LIMITE <=1e-6){
    #   return(
    #     # TODO: best way to raise warning
    #     c(
    #       print(paste("WARNING: we stoped the algorithm because limit is under or equal to: )", 1e-6))
    #     ),
    #     list())
    #   break
    # }
  }
  z=max.col(tik)
  
  return(list(prop=prop,
              mu = mu,
              sigma= sigma,
              alpha = alpha,
              loglik = loglik,
              class = z))
}


# Test
obj <- runEMAlgoritm(data,3,15)
plot(obj$loglik,type='l',main=paste('max loglik :',max(obj$loglik)),cex.main=0.8)
obj$loglik
