library(tools)
source(file_path_as_absolute("functions.R"))

load("webintelligence/compare_estatistico_0406.RData")
load("webintelligence/compareq1_v30.RData")
View(resultados)
dump(resultados, "estatistico.csv")

options(max.print = 99999999)

load("webintelligence/compare_estatistico.RData")
load("webintelligence/compare_estatistico_q3.RData")
View(resultados3)

dump(resultados, "estatistico.csv")

#Q1
load("webintelligence/comparenew.RData")
load("webintelligence/compareq1.RData")

View(resultados3)

#q3
load("webintelligence/compareq3_new.RData")
View(resultados3)



str(resultados2)

dump(resultados, "resultados/jurandir.csv")
dump(resultados2, "resultados/jurandir2.csv")

library(tools)
source(file_path_as_absolute("functions.R"))


library(caret)
library(mlbench)

importance <- varImp(twoGramTypesCFS, scale=FALSE)
# summarize importance
marcos <- print(importance, top = 40)
marcos

print(importance, top = 40)

