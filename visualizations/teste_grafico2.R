library(ggplot2)
df1 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("F1-score", "Precision", "Recall"),2),
                  len=c(89.309, 88.913, 89.709, 91.029, 90.131, 91.946))

df2 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("F1-score", "Precision", "Recall"),2),
                  len=c(82.562, 80.555, 84.671, 87.755, 82.165, 94.160))

df3 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("F1-score", "Precision", "Recall"),2),
                  len=c(76.470, 73.239, 80, 80.141, 74.342, 86.923))


library(ggplot2)
df1 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(89.709, 88.913, 89.309, 91.946, 90.131, 91.029))

df2 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(84.671, 80.555, 82.562, 94.160, 82.165, 87.755))

df3 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(80.000, 73.239, 76.470, 86.923, 74.342, 80.141))

head(df1)
#df1$metric=as.factor(df1$metric)
#df1$metric <- factor(df1$metric, as.character(df1$metric))
df1$metric <- factor(df1$metric, levels = c("Recall", "Precision", "F1-Measure"))
df2$metric <- factor(df2$metric, levels = c("Recall", "Precision", "F1-Measure"))
df3$metric <- factor(df3$metric, levels = c("Recall", "Precision", "F1-Measure"))



ggplot(data=df3, aes(x=metric, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=len), vjust=1.6, color="white",
          position = position_dodge(0.9), size=3.5) +
  labs(title="", x = "", y = "", color = "Categorias", fill = "")


library(forcats)

df1 %>%
  mutate(name = fct_relevel(name, "Recall", "Precision", "F1-Measure")) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity")





# Create some data
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("0.5", "1", "2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


require(ggplot2)

data(ToothGrowth)

df3 <- data_summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))
# Convert dose to a factor variable
df3$dose=as.factor(df3$dose)

head(df3)


# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge())
#+
  #geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
  #              position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + 
  #geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  geom_text(aes(label=len), vjust=1.6, color="black", size=3.5) +
  theme_minimal()


ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

  scale_fill_brewer(palette="Paired")+
  theme_minimal()
  
head(df3)  

df3

ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
    geom_bar(stat="identity", position=position_dodge())+
    #geom_text(aes(label=len), vjust=10, color="white",
  geom_text(aes(label=len), hjust=5, color="white",
              position = position_dodge(0.9), size=3.5)+
    geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                  position=position_dodge(.9)) + 
  
    #scale_fill_brewer(palette="Paired")+
    theme_minimal() + 
    coord_flip()
