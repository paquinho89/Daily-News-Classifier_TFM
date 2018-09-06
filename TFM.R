library(tidyverse)
library(dplyr)
require(rvest)
BBC_News <- read_html('https://www.bbc.com/news/entertainment-arts-45264994')
BBC_News %>% html_nodes('p') %>%  html_text(trim=TRUE)
BBC_News %>% html_nodes('p') %>%  html_text(trim=TRUE) %>% paste(collapse = ' ')
html_node(BBC_News, "title")
#Estuveno intentando coa p√°xina da CNN pero dame  error o scrapping.

#Vamos a descargarnos os tweets de tweeter
# install rtweet from CRAN
install.packages("rtweet")
## load rtweet package
library(rtweet)
#Segue os pasos que se indican nesta p·xina:
#https://rtweet.info/ para quitar os tweets

create_token(
  app = "TwitterMiningAppPaquinho",
  consumer_key = "neLjKSZoj6cSRqSp9686vGbq8",
  consumer_secret = "tqm10cENHprTQtDvvoAUPvqPGXo3WvfL33R5fJzaqluBgbZbvq")

#Get the most recent 3200 tweets from cnn
tweets <- get_timelines(c("bbc"),file_name = "tweets.json", n = 320)
table(tweets$screen_name)

View(tweets)
#WORDCLOUD
#Ver esta paxina http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

txt <- tweets$text
View(txt)
docs <- Corpus(VectorSource(txt))
View(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#Esto È para eliminar os emoticonos
docs <- tm_map(docs, toSpace, "[^\x01-\x7F]")



################################################################
#Vamos a intentar identificar e eliminar os artÌculos, preposicions e demais cousas.
#Botalle un ollo a esta paxina que est· interesante
#https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-usecase-postagging-lemmatisation.html

library("openNLP")
library(NLP)

install.packages("spacyr")
library("spacyr")
sentence <- "My name is Francisco"
##############################################################################################

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("https", "tco")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#The frequency of the first 10 frequent words are plotted :
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

##Vamos a identificar se as palabras son positivas, negativas e que sentimentos nos
##transmiten.
library(tidytext)
View(d)

nrow(get_sentiments("bing"))
nrow(get_sentiments("nrc"))
nrow(get_sentiments("afinn"))
nrow(get_sentiments("loughran"))

#Utilizamos o paquete de NRC porque È o que mais palabras ten e È o que m·is matches.
#ten co teu data set de 'd'.
join <- inner_join(d,get_sentiments("nrc"), by = "word")
View(join)
#Agrupamos e sumamos o n˙mero de ocurrenias de cada categorÌa. Desta forma podes ver
#de que est· falando os tweets. De que categorÌa son cada un.
m<-aggregate(join$freq, by=list(Category=join$sentiment), FUN=sum)
ggplot(m, aes(x=Category, y=x)) + geom_bar(stat = "identity")
View(m) 
