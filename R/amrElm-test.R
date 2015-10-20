
#' Tests the AMR-ELM model.
#' @param XTest the test data, numerical - numerical with zero mean and unit 
#'              variance and patterns in the lines, attributes in the columns
#' @param model the model created by amrElm-train, which contains:
#'          Z: hidden layer weights 
#'          H: hidden layer output
#'          W: output layer weights 
#'          affinity: the affinity used to generate the model (e.g.: cosine affinity)
#'          dataTrain: training data for generating affinity matrix. TODO: try to remove XTrain from 
#'          the parameter list, 
#'          replacing it by the affinity matrix computed for the training patterns
#' @return outupt for the test patterns
#' @export 
#' @examples \dontrun{testOutput <- amrElmTest(XTest, model)}
amrElmTest <- function(XTest, model) {
          
  Z <- model$Z
  H <- model$H
  W <- model$W
  XTrain <- model$dataTrain
  
  nTrain <- dim(XTrain)[1]
  nTest <- dim(XTest)[1]
  
  if (model$affinity=='cosine') {
    affinityMatrix <- cosine(rbind(XTrain,XTest))
  } else {
    stop('Wrong affinity option')
  }
  
  ys <- matrix(0, nrow=1, ncol=nTest)      
  
  #TODO: try to do test in a "block" way, not one-by-one
  for (i in 1:nTest) {
    data <- t(as.matrix(XTest[i,]));
    
    #  Add bias to the test pattern 
    dataAug <- as.matrix(cbind(1,data))
    
    #Test pattern projection
    h <- dataAug %*% Z
    h <- e1071::sigmoid(h)          
    
    #For the test pattern, get the affinity between it and all train patterns
    pl <- c(1, affinityMatrix[nTrain+i, 1:nTrain])
    #Adds the test pattern projection to the train patterns projection matrix (H)
    Hstar <- rbind(h, H); 
    #Calculates the perturbated matrix (pl %*% HStar) for the test pattern (test pattern weighted 
    #by the affinity)
    h <- pl %*% Hstar; 
    
    #Outupt for the test pattern
    ys[i] <- sign(h %*% W);    
  }
  
  return(ys)  
}