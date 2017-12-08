if (!require("FSelector")) {
  install.packages("FSelector")
}
library(FSelector)

data(iris)

colnames(iris)
  
subset <- cfs(Species~., iris)
subset
f <- as.simple.formula(subset, "Species")
print(f)
