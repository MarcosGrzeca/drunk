library(ggplot2)
df1 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(87.673, 89.047, 88.360, 87.517, 92.151, 89.834))

df2 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(89.343, 81.663, 85.503, 96.715, 81.398,89.057))

df3 <- data.frame(supp=rep(c("Baseline", "Proposed Framework"), each=3),
                  metric=rep(c("Recall", "Precision", "F1-Measure"),2),
                  len=c(81.384, 76.191, 78.787, 95.182, 80.892, 88.037))

head(df1)
#df1$metric=as.factor(df1$metric)
#df1$metric <- factor(df1$metric, as.character(df1$metric))
df1$metric <- factor(df1$metric, levels = c("Recall", "Precision", "F1-Measure"))
df2$metric <- factor(df2$metric, levels = c("Recall", "Precision", "F1-Measure"))
df3$metric <- factor(df3$metric, levels = c("Recall", "Precision", "F1-Measure"))



ggplot(data=df1, aes(x=metric, y=len, fill=supp)) +
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
