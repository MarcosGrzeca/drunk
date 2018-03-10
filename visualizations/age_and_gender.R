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

# Plot
email_campaign_funnel$TotalUsers = email_campaign_funnel$totalTweetsQ0
plotPai <- ggplot(email_campaign_funnel, aes(x = agegroup, y = TotalUsers, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  labs(title="Sober",
       x="Age group",
       y="Total of users")+
  scale_fill_brewer(palette = "Dark2")  # Color palette

email_campaign_funnel$TotalUsers = email_campaign_funnel$totalUsersQ1
plotQ1 <- ggplot(email_campaign_funnel, aes(x = agegroup, y = TotalUsers, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  labs(title="Drinking alcohol",
       x="Age group",
       y="Total of users")+
  scale_fill_brewer(palette = "Dark2")  # Color palette

email_campaign_funnel$TotalUsers = email_campaign_funnel$totalUsersQ2
plotQ2 <- ggplot(email_campaign_funnel, aes(x = agegroup, y = TotalUsers, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  labs(title="User drinking alcohol",
       x="Age group",
       y="Total of users")+
  scale_fill_brewer(palette = "Dark2")  # Color palette

email_campaign_funnel$TotalUsers = email_campaign_funnel$totalUsersQ3
plotQ3 <- ggplot(email_campaign_funnel, aes(x = agegroup, y = TotalUsers, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  labs(title="User tweeting while drunk",
       x="Age group",
       y="Total of users")+
  scale_fill_brewer(palette = "Dark2")  # Color palette

library("cowplot")
plot_grid(plotPai, plotQ1, plotQ2, plotQ3, ncol = 2, nrow = 2)