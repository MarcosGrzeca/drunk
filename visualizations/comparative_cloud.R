library(XML)
speechtext <- function(ymd){
  sotu <- data.frame(matrix(nrow=1,ncol=3))
  colnames(sotu) = c("speechtext","year","date")
  for(i in 1:length(ymd)){
    year <- substr(ymd[i],1,4)
    url <- paste0('http://stateoftheunion.onetwothree.net/texts/',ymd[i],'.html')
    doc.html = htmlTreeParse(url, useInternal = TRUE)
    
    doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
    
    # Replace all newline notation with spaces and all quotes with nothing
    doc.text = gsub('\\n', ' ', doc.text)
    doc.text = gsub('\\"', '', doc.text)
    
    doc.text = paste(doc.text, collapse = ' ')
    
    x <- data.frame(doc.text, year, ymd[i], stringsAsFactors = FALSE)
    names(x) <- c("speechtext","year","date")
    sotu <- rbind(sotu, x)
    sotu <- sotu[!is.na(sotu$speechtext), ]
  }
  return(sotu)
}

sotu <- speechtext(c("20080128","20160112"))

sotuData <- as.matrix(sotu)
summary(sotuData)

library(tm)
library(dplyr)
library(xtable)

docs <- Corpus(VectorSource(sotu$speechtext)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()

View(tdm)
colnames(tdm) <- c("Bush","Obama")

print(xtable(head(tdm)), type="html")

library(wordcloud)
#Select Bush speech frequencies and sort in descending order
bushsotu <- as.matrix(tdm[,1])
bushsotu <- as.matrix(bushsotu[order(bushsotu, decreasing=TRUE),])

tdm

print(xtable(head(bushsotu)), type="html")

par(mfrow=c(1,1))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
                 title.size=2.5, max.words=400)
