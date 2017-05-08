#' Creates a model for AMR-ELM.
#'
#' @param l the number of hidden neurons
#' @param XTrain training data, numerical with zero mean and unit variance and patterns in the lines, attributes in the columns
#' @param YTrain training data labels (binary, -1 and +1)
#' @param affinity - only cosine implemented
#' @return the amrElm model for supervised problems, with:
#'          Z: hidden layer weights
#'          H: hidden layer output
#'          W: output layer weights
#'          affinity: the affinity used to generate the model (e.g.: cosine affinity)
#'          dataTrain: training data for generating affinity matrix.
#' @examples
#'
#' \dontrun{
#' library(amrElm)
#'
#' data(heart)
#'
#' data <- heart$data
#' labels <- heart$labels
#'
#' l <- 500
#' N <- nrow(data)
#'
#' randomPatterns <- seq(N)
#' data <- data[randomPatterns,]
#' labels <- labels[randomPatterns]
#'
#' nTrain <- floor(2*N/3)
#' nTest <- N - nTrain
#'
#' data <- data[randomPatterns,]
#' labels <- labels[randomPatterns]
#'
#' XTrain <- data[1:nTrain,]
#' XTest <- data[(nTrain+1):N,]
#'
#' YTrain <- labels[1:nTrain]
#'
#' model <- amrElmTrain(l,XTrain,YTrain)
#'
#' testOutput <- amrElmTest(XTest, model)
#' }
#'
#'@export
amrElmTrain <- function(l, XTrain, YTrain, affinity='cosine') {

  nAttrib <- dim(XTrain)[2]
  nTrain <- dim(XTrain)[1]

  # Get random Z matrix (hidden layer weight matrix)
  Z <- replicate(l, stats::runif(nAttrib+1,-0.5,0.5))

  # Add bias to the XTrain matrix
  Xaug <- as.matrix(cbind(replicate(nTrain,1),XTrain))


  # Apply the sigmoid function to the hidden layer's output matrix (H)
  H <- Xaug %*% Z
  H <- e1071::sigmoid(H)

  if (affinity=='cosine') {
      affinityMatrix <- cosine(XTrain)
  } else {
    stop('Wrong affinity option')
  }


  #Calculates the perturbated matrix (P %*% H) for AMRELM
  HL <-  affinityMatrix %*% H;

  #Calculates the output layer weight matrix (W) and the network output for the train data - AMELM
  W <- corpcor::pseudoinverse(HL) %*% YTrain;
  #YS2 <- sign(HL %*% W2);

  return(list(W=W,Z=Z,H=H,affinity=affinity,dataTrain=XTrain))
}
