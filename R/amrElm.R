
amrElm <- function(x, ...) UseMethod("amrElm")

#' @title
#' AMR-ELM.
#'
#' @description
#' Creates a model for AMR-ELM.
#'
#' @param hidden_neurons the number of hidden neurons
#' @param X training data, numerical with zero mean and unit variance and patterns in the lines, attributes in the columns
#' @param y training data labels (binary, -1 and +1)
#' @param affinity - only cosine implemented
#'
#' @return the amrElm model for supervised problems, with:
#'          Z: hidden layer weights
#'          H: hidden layer output
#'          weights: output layer weights
#'          affinity: the affinity used to generate the model (e.g.: cosine affinity)
#'          dataTrain: training data for generating affinity matrix.
#'
#' @examples
#'
#' data(heart)
#'
#' data <- heart$data
#' labels <- heart$labels
#' n <- nrow(data)
#'
#' split <- caTools::sample.split(labels, SplitRatio = 0.7)
#' train_data <- data[split == TRUE, ]
#' test_data <- data[split == FALSE, ]
#'
#' train_labels <- labels[split == TRUE]
#' test_labels <- labels[split == FALSE]
#'
#' model <- amrElm(train_data, train_labels, hidden_neurons = 500)
#' predicted_labels <- predict(model, test_data)
#'
#' @export

amrElm.default <- function(X, y, hidden_neurons,
                           affinity = c("cosine", "none")) {

  affinity <- match.arg(affinity)
  n <- nrow(X)
  p <- ncol(X)

  # Get random Z matrix (hidden layer weight matrix)
  Z <- replicate(hidden_neurons, runif(p + 1, -0.5, 0.5))

  # Add bias to the X matrix
  X_aug <- as.matrix(cbind(replicate(n, 1), X))

  # Apply the sigmoid function to the hidden layer's output matrix (H)
  H <- X_aug %*% Z
  H <- e1071::sigmoid(H)

  if (affinity == "cosine") {
    affinity_matrix <- cosine(X)
  } else {
    stop("amrElm: Wrong affinity option")
  }

  # Calculates the perturbated matrix (P %*% H) for AMRELM
  HL <- affinity_matrix %*% H

  # Calculates the output layer weight matrix (weights) and the network
  # output for the train data - AMELM
  weights <- corpcor::pseudoinverse(HL) %*% y

  return(structure(
    list(weights = weights,
         Z = Z,
         H = H,
         affinity = affinity,
         X = X),
    class = "amrElm"
  ))
}

#' @title
#' AMR-ELM.
#'
#' @description
#' Tests the AMR-ELM model.
#'
#' @param newdata the test data, numerical - numerical with zero mean and unit
#'              variance and patterns in the lines, attributes in the columns
#' @param model the model created by amrElm-train, which contains:
#'          Z: hidden layer weights
#'          H: hidden layer output
#'          weights: output layer weights
#'          affinity: the affinity used to generate the model (e.g.: cosine affinity)
#'          dataTrain: training data for generating affinity matrix. TODO: try to remove X from
#'          the parameter list,
#'          replacing it by the affinity matrix computed for the training patterns
#' @return outupt for the test patterns
#' @export
#' @examples \dontrun{testOutput <- amrElmTest(newdata, model)}

predict.amrElm <- function(model, newdata) {

  Z <- model$Z
  H <- model$H
  weights <- model$weights
  X <- model$X

  n_train <- dim(X)[1]
  n_test <- dim(newdata)[1]

  if (model$affinity == "cosine") {
    affinity_matrix <- cosine(rbind(X, newdata))
  } else {
    stop("Wrong affinity option")
  }

  ys <- matrix(0, nrow = 1, ncol = n_test)

  #TODO: try to do test in a "block" way, not one-by-one
  for (i in 1:n_test) {
    data <- t(as.matrix(newdata[i, ]));

    #  Add bias to the test pattern
    data_aug <- as.matrix(cbind(1, data))

    #Test pattern projection
    h <- data_aug %*% Z
    h <- e1071::sigmoid(h)

    # For the test pattern, get the affinity between it
    # and all train patterns
    pl <- c(1, affinity_matrix[n_train + i, 1:n_train])

    # Adds the test pattern projection to the train patterns
    # projection matrix (H)
    H_star <- rbind(h, H);

    # Calculates the perturbated matrix (pl %*% HStar) for the test
    # pattern (test pattern weighted by the affinity)
    h <- pl %*% H_star;

    #Outupt for the test pattern
    ys[i] <- sign(h %*% weights);
  }

  return(ys)
}
