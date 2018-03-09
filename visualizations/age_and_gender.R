library(ggplot2)
library(ggthemes)
options(scipen = 999)  # turns of scientific notations like 1e+40

# Read data
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

email_campaign_funnel <- read.csv("visualizations/marcos.csv")
email_campaign_funnel$faixa

# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
#lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

email_campaign_funnel$TotalUsers = email_campaign_funnel$totalUsers

# Plot
ggplot(email_campaign_funnel, aes(x = agegroup, y = TotalUsers, fill = Gender)) +   # Fill column

#ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  #scale_y_continuous(breaks = brks,   # Breaks
#                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  labs(title="Users by Gender and Age group",
       x="Age group",
       y="Total of users")+
  scale_fill_brewer(palette = "Dark2")  # Color palette