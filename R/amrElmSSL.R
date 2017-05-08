
amrElmSSL <- function(x, ...) UseMethod("amrElmSSL")

#' Creates a model for AMR-ELM.
#'
#' @param hidden_neurons the number of hidden neurons
#' @param nl the number of labeled patterns
#' @param X training data, numerical with zero mean and unit variance and patterns in the lines, attributes in the columns
#'        - the unlabeled patterns must came after the labeled ones
#' @param y training data labels (only two classes, with labels equals to -1 or +1, and 0 for the unlabeled patterns)
#' @param affinity - only cosine implemented
#' @return The amrElm model for semissupervised problems - a list with:
#'          Z: hidden layer weights
#'          H: hidden layer output
#'          weights: output layer weights
#'          affinity: the affinity used to generate the model (e.g.: cosine affinity)
#'          X: training data for generating affinity matrix.
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
#' hidden_neurons <- 500
#' nl <- 50
#'
#' N <- nrow(data)
#' randomPatterns <- seq(N)
#'
#' data <- data[randomPatterns,]
#' labels <- labels[randomPatterns]
#'
#' n <- floor(2*N/3)
#' nTest <- N - n
#'
#' data <- data[randomPatterns,]
#' labels <- labels[randomPatterns]
#'
#' X <- data[1:n,]
#' XTest <- data[(n+1):N,]
#'
#' y <- labels[1:n]
#' y[(nl+1):n] <- 0
#'
#' model <- amrElmSSLTrain(hidden_neurons,nl,X,y)
#' testOutput <- amrElmTest(XTest, model)
#' }
#'
#' @export

amrElmSSL.default <- function(X, y, hidden_neurons, nl,
                              affinity="cosine") {
  n <- nrow(X)
  p <- ncol(X)

  # Get random Z matrix (hidden layer weight matrix)
  Z <- replicate(hidden_neurons, stats::runif(p + 1, -0.5, 0.5))

  # Add bias to the X matrix
  X_aug <- as.matrix(cbind(replicate(n, 1), X))

  # Apply the sigmoid function to the hidden layer's output matrix (H)
  H <- X_aug %*% Z
  H <- e1071::sigmoid(H)

  if (affinity == "cosine") {
    affinity_matrix <- cosine(X)
  } else {
    stop("Wrong affinity option")
  }

  # Calculates the perturbated matrix (P %*% H) for AMRELM
  HL <-  affinity_matrix %*% H;

  # Calculates the output layer weight matrix (weights) and
  # the network output for the train data - AMELM
  weights <- corpcor::pseudoinverse(HL[1:nl, ]) %*% y[1:nl];

  return(structure(
    list(weights = weights,
         Z = Z,
         H = H,
         affinity = affinity,
         X = X),
    class = "amrElmSSL"
  ))
}

predict.amrElmSSL <- function(model, newdata) {
  predict.amrElm(model, newdata)
}
