library(ggplot2)
df1 <- data.frame(supp=rep(c("Sober", "Drunk"), each=8),
                  metric=rep(c("Joy", "Fear", "Sadness", "Anger", "Anticipation", "Disgust", "Surprise", "Trust"),2),
                  len=c(12,15,14,15,12,7,13,12,25,8,8,11,16,8,9,14))

head(df1)
df1$metric <- factor(df1$metric, levels = c("Joy", "Fear", "Sadness", "Anger", "Anticipation", "Disgust", "Surprise", "Trust")  )
df1$supp= factor(df1$supp,levels=sort(levels(df1$supp), TRUE))


ggplot(data=df1, aes(x=metric, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="", x = "", y = "", color = "Categorias", fill = "") +
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3) +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(legend.position="bottom")



ggplot(data=df1, aes(x=metric, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="", x = "", y = "", color = "Categorias", fill = "") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  coord_flip()




#scale_fill_brewer(palette="Paired")

