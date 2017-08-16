
install.packages("openNLP")
#install.packages("openNLPdata")
#install.packages("openNLPmodels.en")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

install.packages("tm")
install.packages("stringr")
install.packages("gsubfn")
install.packages("plyr")

install.packages("foo", repos = "http://datacube.wu.ac.at/", type = "source")

library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)
library(stringr)
library(gsubfn)
library(plyr)

#library(openNLPmodels.en)


acq <- "This paper describes a novel optical thread plug
gauge (OTPG) for internal thread inspection using machine
vision. The OTPG is composed of a rigid industrial
endoscope, a charge-coupled device camera, and a two
degree-of-freedom motion control unit. A sequence of
partial wall images of an internal thread are retrieved and
reconstructed into a 2D unwrapped image. Then, a digital
image processing and classification procedure is used to
normalize, segment, and determine the quality of the
internal thread." 

acqTag <- tagPOS(acq)     
acqTagSplit = strsplit(acqTag," ")

installed.packages()

packageVersion("devtools")

install.packages("pacman")





require("NLP")
## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")

s <- "@_illestOut_: @NaiShekirah I graduate this year turn up\"aye me too , Turnup . Class 2014"

s <- as.String(s)

## Chunking needs word token annotations with POS tags.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s,
               list(sent_token_annotator,
                    word_token_annotator,
                    pos_tag_annotator))
ano <- annotate(s, Maxent_Chunk_Annotator(), a3)
ano

ano[2]
length(ano)


#annotate(s, Maxent_Chunk_Annotator(probs = TRUE), a3)

fore <- function(x) {
  print(as.String(x[2]))
  
}
apply(ano, 1, fore)

require("NLP")
## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
parse_annotator <- Parse_Annotator()
## Compute the parse annotations only.
p <- parse_annotator(s, a2)
## Extract the formatted parse trees.
ptexts <- sapply(p$features, `[[`, "parse")
ptexts
## Read into NLP Tree objects.
ptrees <- lapply(ptexts, Tree_parse)
ptrees

#https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html