
context("amrElmTrain")

test_that("amrElmTrain unit test", {

  data(heart)
  data <- heart$data
  labels <- heart$labels
  l <- 500
  N <- nrow(data)
  randomPatterns <- seq(N)
  data <- data[randomPatterns,]
  labels <- labels[randomPatterns]
  nTrain <- floor(2*N/3)
  nTest <- N - nTrain
  data <- data[randomPatterns,]
  labels <- labels[randomPatterns]
  XTrain <- data[1:nTrain,]
  XTest <- data[(nTrain+1):N,]
  YTrain <- labels[1:nTrain]
  model <- amrElmTrain(l,XTrain,YTrain)
  testOutput <- amrElmTest(XTest, model)

})

test_that("amrElm-ssl-train unit test", {

  data(heart)
  data <- heart$data
  labels <- heart$labels
  l <- 500
  nl <- 50
  N <- nrow(data)
  randomPatterns <- seq(N)
  data <- data[randomPatterns,]
  labels <- labels[randomPatterns]
  nTrain <- floor(2*N/3)
  nTest <- N - nTrain
  data <- data[randomPatterns,]
  labels <- labels[randomPatterns]
  XTrain <- data[1:nTrain,]
  XTest <- data[(nTrain+1):N,]
  YTrain <- labels[1:nTrain]
  YTrain[(nl+1):nTrain] <- 0
  model <- amrElmSSLTrain(l,nl,XTrain,YTrain)
  testOutput <- amrElmTest(XTest, model)

})
