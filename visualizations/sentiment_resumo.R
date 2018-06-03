library(ggplot2)
theme_set(theme_classic())

source_df <- read.csv("visualizations/sentiment_type.csv")

library("cowplot")

#install.packages("waffle")
library(waffle)

header <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

tmp <- transform(source_df, transform=totalTweetsNaoAlcoolizados/12)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotTweetsNaoAlcoolizados <- waffle(vals, title="Sober")

tmp <- transform(source_df, transform=totalTweetsQ1/12)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ1 <- waffle::waffle(vals, title="Drinking alcohol")

tmp <- transform(source_df, transform=totalTweetsQ2/12)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ2 <- waffle::waffle(vals,  title="Drunk")

tmp <- transform(source_df, transform=totalTweetsQ3/12)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ3 <- waffle::waffle(vals, title="User tweeting while drunk", pad=1)

#waffle(parts, rows=8)
#waffle(parts, rows=8, colors=c("#969696", "#1879bf", "#009bda"), legend_pos="bottom") 

library("cowplot")
#plot_grid(plotTweetsNaoAlcoolizados, plotQ1, plotQ2, plotQ3, labels = c("Question 1-", "Question 1+", "Question 2+", "Question 3+"), ncol = 2, nrow = 2)

plot_grid(plotTweetsNaoAlcoolizados, plotQ2, ncol = 1, nrow = 2)



#https://github.com/hrbrmstr/waffle

