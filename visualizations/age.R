library(ggplot2)
theme_set(theme_classic())

# Source: Frequency table

#df <- as.data.frame(table(mpg$class))
#colnames(df) <- c("class", "freq")
#pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
#  geom_bar(width = 1, stat = "identity") +
#  theme(axis.line = element_blank(), 
#        plot.title = element_text(hjust=0.5)) + 
#  labs(fill="class", 
#       x=NULL, 
#       y=NULL, 
#       title="Pie Chart of class", 
#       caption="Source: mpg")

#pie + coord_polar(theta = "y", start=0)

# Source: Categorical variable.
# mpg$class
#pie <- ggplot(mpg, aes(x = "", fill = factor(class))) + 

#pie <- ggplot(mpg, aes(x = "", fill = factor(class))) +
#  geom_bar(width = 1) +
#  theme(axis.line = element_blank(), 
#        plot.title = element_text(hjust=0.5)) + 
#  labs(fill="class", 
#       x=NULL, 
#       y=NULL, 
#       title="Pie Chart of class", 
#       caption="Source: mpg")

#pie <- pie + coord_polar(theta = "y", start=0)

source_df <- read.csv("visualizations/age.csv")

pieTotal <- ggplot(source_df, aes(x = "", y=totalTweets, fill = factor(faixa))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  coord_polar(theta = "y", start=0) +
  labs(fill="faixa", 
       x=NULL, 
       y="MARCOS", 
       title="Total of tweets")

pieTotal

pieQ1 <- ggplot(source_df, aes(x = "", y=totalTweetsQ1, fill = factor(faixa))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="faixa", 
       x=NULL, 
       y=NULL, 
       title="Question 1+")

pieQ1 <- pieQ1 + coord_polar(theta = "y", start=0)

pieQ2 <- ggplot(source_df, aes(x = "", y=totalTweetsQ2, fill = factor(faixa))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="faixa", 
       x=NULL, 
       y=NULL, 
       title="Question 2+")

pieQ2 <- pieQ2 + coord_polar(theta = "y", start=0)

pieQ3 <- ggplot(source_df, aes(x = "", y=totalTweetsQ3, fill = factor(faixa))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="faixa", 
       x=NULL, 
       y=NULL, 
       title="Question 3+")

pieQ3 <- pieQ3 + coord_polar(theta = "y", start=0)


library("cowplot")
plot_grid(pieTotal, pieQ1, pieQ2, pieQ3, labels = c("Total", "Question 1", "Q2", "Q3"), ncol = 2, nrow = 2)


#install.packages("waffle")
library(waffle)

header <- c("<=17", "18-24", "25-34", "35-44", "45-54", ">=55")

tmp <- transform(source_df, transform=totalTweets/15)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotPai <- waffle::waffle(vals) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

tmp <- transform(source_df, transform=totalTweetsQ1/9)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ1 <- waffle::waffle(vals) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

tmp <- transform(source_df, transform=totalTweetsQ2/5)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ2 <- waffle::waffle(vals) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

tmp <- transform(source_df, transform=totalTweetsQ3/2)
vals <- c(tmp$transform)
val_names <- sprintf("%s (%s)", header, scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names
plotQ3 <- waffle::waffle(vals) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#waffle(parts, rows=8)
#waffle(parts, rows=8, size=1, colors=c("#969696", "#1879bf", "#009bda"), legend_pos="bottom") 

library("cowplot")
plot_grid(plotPai, plotQ1, plotQ2, plotQ3, labels = c("Total", "Question 1+", "Question 2+", "Question 3+"), ncol = 2, nrow = 2)


#https://github.com/hrbrmstr/waffle

library(ggplot2)
library(emojifont)
load.fontawesome()

library(extrafont)
waffle(vals, rows=3, colors=c("#969696", "#1879bf", "#009bda"),
       use_glyph="medkit", size=8)



install.packages("https://github.com/FortAwesome/Font-Awesome/tree/master/fonts")


install_github("FortAwesome/Font-Awesome")
githubinstall("PackageName")


install.packages("devtools")
devtools::install_github("FortAwesome/Font-Awesome")


install.packages("extrafontdb", repos = "http://cran.rstudio.com/")

fa_font <- tempfile(fileext = ".ttf")
download.file("http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/fonts/fontawesome-webfont.ttf?v=4.3.0",
              destfile = fa_font, method = "curl")


font_import(paths = dirname(fa_font), prompt = FALSE)



install.packages("emojifont")
library("emojifont")
load.fontawesome()
fa <- fontawesome(c('fa-github', 'fa-weibo', 'fa-twitter', 'fa-android', 'fa-coffee'))
fa


set.seed(2016-03-09)
fa <- fontawesome(c('fa-smile', 'fa-weibo', 'fa-twitter', 'fa-android', 'fa-coffee'))
d <- data.frame(x=rnorm(20), 
                y=rnorm(20), 
                label=sample(fa, 20, replace=T))

ggplot(d, aes(x, y, color=label)) + 
  geom_text(aes(label=label), family='fontawesome-webfont')+
  xlab(NULL)+ylab(NULL)
  
library(emojifont)
search_emoji('smile')
emoji(search_emoji('smile'))

set.seed(123)
x <- rnorm(10)
set.seed(321)
y <- rnorm(10)
plot(x, y, cex=0)
text(x, y, labels=emoji('cow'), cex=1.5, col='steelblue', family='EmojiOne')


d <- data.frame(x=x, y=y,
                label = sample(c(emoji('cow'), emoji('camel')), 10, replace=TRUE),
                type = sample(LETTERS[1:3], 10, replace=TRUE))

library("ggplot2")
ggplot(d, aes(x, y, color=type, label=label)) +
  geom_text(family="EmojiOne", size=6)

