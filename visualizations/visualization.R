options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id, q2 AS resposta, textParser, textoParserEmoticom AS textoCompleto, hashtags, emoticonPos,	emoticonNeg FROM tweets t WHERE textparser <> '' AND id <> 462478714693890048 AND q1 = 1 AND q2 IS NOT NULL")
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textParser <- enc2utf8(dados$textParser)
dados$hashtags <- enc2utf8(dados$hashtags)
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)


library(ggplot2)
library(scales)
theme_set(theme_classic())

# prep data
df <- read.csv("planilhas/gender_copy.csv")
colnames(df) <- c("sexo", "totalTweetsQ1", "totalTweetsQ2")
left_label <- paste(df$sexo, round(df$`totalTweetsQ1`),sep=", ")
right_label <- paste(df$sexo, round(df$`totalTweetsQ2`),sep=", ")
df$class <- ifelse((df$`totalTweetsQ2` - df$`totalTweetsQ1`) < 0, "red", "green")

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`totalTweetsQ1`, yend=`totalTweetsQ2`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="Tweets for gender") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`totalTweetsQ1`, df$`totalTweetsQ2`))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=df$`totalTweetsQ1`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`totalTweetsQ2`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="Question 1", x=1, y=1.1*(max(df$`totalTweetsQ1`, df$`totalTweetsQ2`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Question 2", x=2, y=1.1*(max(df$`totalTweetsQ1`, df$`totalTweetsQ2`)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))




df <- read.csv("planilhas/gender_copy2.csv")
colnames(df) <- c("question", "male", "female")
left_label <- paste(df$question, round(df$`male`),sep=", ")
right_label <- paste(df$question, round(df$`female`),sep=", ")
df$class <- ifelse((df$`female` - df$`male`) < 0, "red", "green")

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`male`, yend=`female`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="Tweets for gender") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`male`, df$`female`))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=df$`male`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`female`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="Male", x=1, y=1.1*(max(df$`male`, df$`female`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Female", x=2, y=1.1*(max(df$`male`, df$`female`)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))


par(mfrow=c(1,2))
p1962= c(16,30,28,15,3,6)
lbl1962=c("Female","Male","ND")
pie(p1962, labels=lbl1962, main ="Total of Twets")
p2007=c(36,29,16,9,8,3)
lbl2007=c("a","b","c","d","e","f")
pie(p1962, labels=lbl2007, main = "Question 1")












library(dplyr)
theme_set(theme_classic())
source_df <- read.csv("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/cancer_survival_rates.csv")

# Define functions. Source: https://github.com/jkeirstead/r-slopegraph
tufte_sort <- function(df, x="year", y="value", group="group", method="tufte", min.space=0.05) {
  ## First rename the columns for consistency
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  ## Start at "bottom" row
  ## Repeat for rest of the rows until you hit the top
  for (i in 2:nrow(tmp)) {
    ## Shift subsequent row up by equal space so gap between
    ## two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
  }
  
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}

plot_slopegraph <- function(df) {
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- 3
  gg <- ggplot(df,aes(x=x,y=ypos)) +
    geom_line(aes(group=group),colour="grey80") +
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y), size=fontSize, family="American Typewriter") +
    scale_y_continuous(name="", breaks=yvals, labels=ylabs)
  return(gg)
}    

## Prepare data    
df <- tufte_sort(source_df, 
                 x="year", 
                 y="value", 
                 group="group", 
                 method="tufte", 
                 min.space=0.05)

df <- transform(df, 
                x=factor(x, levels=c(5,10,15,20), 
                         labels=c("5 years","10 years","15 years","20 years")), 
                y=round(y))

## Plot
plot_slopegraph(df) + labs(title="Estimates of % survival rates") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "American Typewriter",
                                  face="bold"),
        axis.text = element_text(family = "American Typewriter",
                                 face="bold"))

