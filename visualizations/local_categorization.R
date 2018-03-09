library(ggplot2)
theme_set(theme_bw())

df <- read.csv("visualizations/location_q1.csv")
df$Category <- factor(df$Categoria, levels = df$Categoria)  # to retain the order in plot.

head(df, 20)

# Draw plot
ggplot(head(df, 15), aes(x=Category, y=Total)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Tweets organized by category of localization") + 
  theme(axis.text.x = element_text(angle=80, vjust=0.6))
