resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Baseline", "F1", "Precisão", "Revocação")

addRow <- function(resultados, baseline, matriz, ...) {
  print(baseline)
  newRes <- data.frame(baseline, matriz$byClass["F1"], matriz$byClass["Precision"], matriz$byClass["Recall"])
  rownames(newRes) <- baseline
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  #save.image(file="2110/rdas/testes2605.RData")
  return (newdf)
}


n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b) 


l_tweets_stats <- count(l_tweets[l_tweets$country %in% c('Brazil', 'Canada', 'Mexico', 'United Kingdom', 'United States'), ], Campaign, country)

#Criando o percentual por pais
l_tweets_stats$n_country <- NA
l_tweets_stats$n_country <- ave(l_tweets_stats$n, l_tweets_stats$country, FUN=sum)
l_tweets_stats$n_country_perc <- NA
l_tweets_stats$n_country_perc <- (l_tweets_stats$n / l_tweets_stats$n_country) * 100


p <- ggplot(data = l_tweets_stats, aes(x = country, y = n_country_perc, fill = Campaign))
p <- p + geom_bar(stat = "identity", position = "dodge", colour = "black")
p <- p + labs(x = "Country", y = "Total Tweets")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + scale_y_continuous(labels = scales::unit_format("%"))
p <- p + guides(fill=guide_legend(title="Campaign"))
p <- p + scale_fill_manual(values=c("#56B4E9", "pink")) 
p


require(stringr)
df <- data.frame(col1 = c("ABCDEFGH ALKABCDEFGHALKABCDEFGH", "ALKABCDEFGHALKABCDEFGH OPHOPH", "OPHOPH", "BCZ", "LKH", "QRQ", "AAA", "VYY"))
df$col1
str_sub(df$col1, 1, 10)


modificado <- function() {
	l_tweets_stats <- count(l_tweets[l_tweets$country %in% c('Brazil', 'Canada', 'Mexico', 'United Kingdom', 'United States'), ], Campaign, country)

	#Criando o percentual por pais
	l_tweets_stats$n_country <- NA
	l_tweets_stats$n_country <- ave(l_tweets_stats$n, l_tweets_stats$country, FUN=sum)
	l_tweets_stats$n_country_perc <- NA
	l_tweets_stats$n_country_perc <- (l_tweets_stats$n / l_tweets_stats$n_country) * 100


	p <- ggplot(data = l_tweets_stats, aes(x = country, y = n_country_perc, fill = Campaign,  label = 'métrica do y'))
	p <- p + geom_bar(stat = "identity", position = "dodge", colour = "black")
	p <- p + Geom_text(position = "stack")
	p <- p + labs(x = "Country", y = "Total Tweets")
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	p <- p + scale_y_continuous(labels = scales::unit_format("%"))
	p <- p + guides(fill=guide_legend(title="Campaign"))
	p <- p + scale_fill_manual(values=c("#56B4E9", "pink")) 
	p
}