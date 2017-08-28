library(e1071)
model <- svm(Y ~ X , data)

predictedY <- predict(model, data)

points(data$X, predictedY, col = "red", pch=4)