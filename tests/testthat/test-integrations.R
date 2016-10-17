
context("Integration Test")

test_that("Newer and older versions produce the same predictions for amrElm.", {

  data(heart)

  data <- heart$data
  labels <- heart$labels

  n <- nrow(data)

  split <- caTools::sample.split(labels, SplitRatio = 0.7)

  train_data <- data[split == TRUE, ]
  test_data <- data[split == FALSE, ]

  train_labels <- labels[split == TRUE]
  test_labels <- labels[split == FALSE]

  model_1 <- amrElm(train_data, train_labels, hidden_neurons = 500)
  predicted_labels_1 <- predict(model_1, test_data)

  model_2 <- amrElmTrain(l = 500, train_data, train_labels)
  predicted_labels_2 <- amrElmTest(test_data, model_2)

  expect_true(all(predicted_labels_1 == predicted_labels_2))
})


test_that("Newer and older versions produce the same predictions for amrElmSSL.", {

  data(heart)

  data <- heart$data
  labels <- heart$labels

  n <- nrow(data)

  split <- caTools::sample.split(labels, SplitRatio = 0.7)

  train_data <- data[split == TRUE, ]
  test_data <- data[split == FALSE, ]

  train_labels <- labels[split == TRUE]
  test_labels <- labels[split == FALSE]

  model_1 <- amrElmSSL(train_data, train_labels, hidden_neurons = 500, nl = 50)
  predicted_labels_1 <- predict(model_1, test_data)

  model_2 <- amrElmSSLTrain(train_data, train_labels, l = 500, nl = 50)
  predicted_labels_2 <- amrElmTest(test_data, model_2)

  expect_true(all(predicted_labels_1 == predicted_labels_2))
})
