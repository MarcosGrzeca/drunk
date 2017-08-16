library(tools)
source(file_path_as_absolute("functions.R"))


library(caret)
library(mlbench)


load("resultados/fit.Rda")
importance <- varImp(fit, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance, top = 30)
