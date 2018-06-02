load(file="2110/pruning/rda/information-gain-concept-semantic-q2.RData")

weights


library(FSelector)
subset <- cutoff.k(weights, 4)
subset
f <- as.simple.formula(subset, "resposta")

subset <- cutoff.biggest.diff(weights)
subset

print(f)


data <- as.data.frame(weights)
newdata <- data[order(attr_importance),] 


attach(mtcars)
str(mtcars)
