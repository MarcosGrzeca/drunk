load("webintelligence/comparenew.RData")
View(resultados3)

dump(resultados, "resultados/jurandir.csv")
dump(resultados2, "resultados/jurandir2.csv")

library(tools)
source(file_path_as_absolute("functions.R"))


library(caret)
library(mlbench)

importance <- varImp(twoGramTypesCFS, scale=FALSE)
# summarize importance
marcos <- print(importance, top = 31)
marcos

ls()
