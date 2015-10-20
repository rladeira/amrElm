\name{amrElm-package}
\alias{amrElm-package}
\alias{amrElm}
\docType{package}
\title{
Implementation of AMR-ELM (Affinity Matrix Regularized Extreme Learning Machine) algorithm.
}
\description{
This package implements a regularized version of ELM. Regularization is performed using a priori spacial information expressed by an affinity matrix. The use of this type of a priori information is similar to perform Tikhonov regularization. Furthermore, if a parameter free affinity matrix is used, like the cosine similarity matrix, regularization is performed without any need for parameter tunning. This version
is applicable for binary classification problems.
}
\details{
\tabular{ll}{
Package: \tab amrElm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-03\cr
License: \tab MIT \cr
}
To create the model, the function to use is \code{amrElmTrain}. To predict values (or to test the
model), the function to use is \code{amrElmTest}.
}
\author{
Leonardo José Silvestre

Maintainer: Leonardo José Silvestre <leonardo.silvestre@ufes.br>
}
\references{
SILVESTRE, L. J. ; LEMOS, A. P. ; BRAGA, J. P. ; Braga, A. P. . \emph{Dataset Structure as Prior 
Information for Parameter-Free Regularization of Extreme Learning Machines} In: \emph{Neurocomputing (accepted for publication)}.\cr

SILVESTRE, L. J. ; LEMOS, A. P. ; BRAGA, J. P. ; Braga, A. P. . \emph{Parameter-free regularization in
Extreme Learning Machines with affinity matrices.} In: \emph{Proceedings of the European
Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning},
ESANN 2014, 23-25 April 2014, Bruges, Belgium. Louvain-la-Nueve, Belgique, 2014. p. 595-600.


}
\keyword{ package }
%\usage{}
%\examples{}
