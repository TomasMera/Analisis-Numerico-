#Ejemplo 2: Text mining y Machile learning sobre datos de Twitter
#Modelado de temas sobre los datos de Twitter

library(twitteR)
library(tm)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(syuzhet)
library(sqldf)
library(topicmodels)

options(sqldf.driver = "SQLite") 

#limpio la memoria
gc()


# Claves de twitter 
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""


#Conecto con la api de twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#searchTwitter: busqueda de tweets a partir de ciertos parametros


tweet_corpus2 <- searchTwitter('#iPhone13 OR #iPhone13Pro',  
                              n = 10000, lang = 'en', resultType = 'recent')


#userTimeline: publicaciones de un usuario
tweets_us_cnn <- twListToDF(userTimeline('CNN',  n = 1000))  
#tweets_us_netflix <- twListToDF(userTimeline('Netflix',  n = 1000)) #NetflixLAT


#getUser: informacion de un usuario
#user <- getUser('')
#userFriends <- user$getFriends()
#userFollowers <- user$getFollowers()

#busqueda de tendencias por localidad
#localidades <- availableTrendLocations()
tendendias_arg <- getTrends(woeid = 23424747) #23424747: Argentina, #2459115: ny, 1: mundo
tendencias_mundo <- getTrends(woeid = 1) #23424747: Argentina, #2459115: ny, 1: mundo


#Elimino los retweets
#tweet_corpus <- strip_retweets(tweet_corpus)

#Guardo los tweets en un data frame
tweet_df <- twListToDF(tweet_corpus)
tweets <- tweet_df$text

#Guardo los datos en un csv
#write.csv(tweet_df, 'C:/twitter_iphone.csv')


#Leo el csv con los tweets ya descargados
tweet_df <- read.csv("C:/twitter_iphone.csv")
#tweets <- iconv(tweet_df$text, to = "ASCII")#, sub = " ") 
tweets <-tweet_df$text

#Analisis de sentimiento
sent_syuzhet <- get_sentiment(tweets, method="syuzhet")
sent_bing <- get_sentiment(tweets, method="bing")
sent_afinn <- get_sentiment(tweets, method="afinn")
sent_nrc <- get_sentiment(tweets, method="nrc")
nrc_data <- get_nrc_sentiment(tweets)

#Analisis de sentimiento en espa?ol
#sent_syuzhet <- get_sentiment(tweets, method="syuzhet", language="spanish")
#sent_bing <- get_sentiment(tweets, method="bing", language="spanish")
#sent_afinn <- get_sentiment(tweets, method="afinn", language="spanish")
#sent_nrc <- get_sentiment(tweets, method="nrc", language="spanish")
#nrc_data <- get_nrc_sentiment(tweets, language="spanish")

#para ver como trabaja cada libreria los sentimientos
#library(tidytext)
#sentimientos <- get_sentiments(lexicon = "nrc")
#head(sentimientos)

#Se agrega el analisis de sentimiento al dataframe
tweet_df <- cbind(tweet_df, tweets, sent_syuzhet, sent_bing, sent_afinn, sent_nrc, nrc_data)

#sqldf()
#tweet_df_neg <- sqldf("select * 
#                      from tweet_df  
#                      where sent_afinn < -5
#                        --and sent_syuzhet > 0 
#                        --and sent_bing > 0 
#                        --and sent_afinn > 0 
#                        --and sent_nrc > 0
#                      ")


#tweets_positivos <- iconv(tweet_df_pos$text, to = "ASCII")#, sub = " ") 

sqldf()
sentimientos <- sqldf("select sum(case when sent_syuzhet> 0 then 1 else 0 end) as Cantidad,  'pos_syuzhet' as Sentimiento from tweet_df
                      union select sum(case when sent_syuzhet< 0 then 1 else 0 end) ,'neg_syuzhet' from tweet_df
                      union select sum(case when sent_bing> 0 then 1 else 0 end) ,'pos_bing' from tweet_df
                      union select sum(case when sent_bing< 0 then 1 else 0 end), 'neg_bing' from tweet_df
                      union select sum(case when sent_afinn> 0 then 1 else 0 end), 'pos_afinn' from tweet_df
                      union select sum(case when sent_afinn< 0 then 1 else 0 end), 'neg_afinn' from tweet_df
                      union select sum(case when sent_nrc> 0 then 1 else 0 end), 'pos_nrc' from tweet_df 
                      union select sum(case when sent_nrc< 0 then 1 else 0 end), 'neg_nrc' from tweet_df
                      ")

sqldf()
emociones <- sqldf("select sum(joy) as Cantidad, 'Alegria' as Emocion from tweet_df 
                      union select sum(fear) , 'Miedo'from tweet_df
                      union select sum(anger), 'Ira' from tweet_df
                      union select sum(sadness),  'Tristeza' from tweet_df
                      union select sum(trust) , 'Confianza' from tweet_df 
                      union select sum(surprise) , 'Sorpresa' from tweet_df
                      union select sum(disgust), 'Disgusto' from tweet_df
                      union select sum(anticipation), 'Anticipacion' from tweet_df
                      ")

#Sentimientos
graf_sentimientos<-ggplot(data=sentimientos, aes(x=reorder(Sentimiento, -Cantidad), y=Cantidad, fill=Sentimiento)) +
  geom_bar(stat="identity") +coord_flip()+
  geom_text(aes(label=Cantidad),  color="black", size=3.5)
graf_sentimientos

#Emociones
graf_emociones<-ggplot(data=emociones, aes(x=reorder(Emocion, -Cantidad), y=Cantidad, fill=Emocion)) +
  geom_bar(stat="identity") +coord_flip()+
  geom_text(aes(label=Cantidad),  color="black", size=3.5)
graf_emociones


#Se cargan los datos como un corpus (con VCorpus no tira warning)
corpus <- VCorpus(VectorSource(tweets))
                  
#Limpieza de datos
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
toSpace <- function (x , pattern ) gsub(pattern, " ", x)

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(toSpace), "/")
corpus <- tm_map(corpus, content_transformer(toSpace), "@")
corpus <- tm_map(corpus, content_transformer(toSpace), "\\|")
corpus <- tm_map(corpus, content_transformer(toSpace), "-")
corpus <- tm_map(corpus, content_transformer(toSpace), "'")
corpus <- tm_map(corpus, content_transformer(toSpace), ".")
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords('english')) 
corpus <- tm_map(corpus, removeWords, c("amp","xbox", "playstat", "play", "game", "will", "awsiiq", "win", "look", "willi", "wonka", "this", "the", "for", "you", "soon", "phone", "iphon", "mobil", "netflix", "apple", "appl", "get", "\U0001f6a8", "-","...", "\U0001f389", "-",".", "'","awsiiq" ))


# Matriz de terminos del doc
tdm <- TermDocumentMatrix(corpus)
#tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=300)
df <- data.frame(term = names(term.freq), freq = term.freq)

graf_freq_palabras <- ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity", fill="#f70388") + xlab("Palabras") + ylab("Cant") +coord_flip()
graf_freq_palabras

#df

#Asociaciones entre palabras
#findAssocs(tdm, "screen", 0.3)

##Nube de palabras
#si falla el grafico, limpio los plots: graphics.off()
wordcloud(corpus, max.words = 100, random.order = FALSE, scale=c(2,.5),
          col = brewer.pal(8, "Dark2"))


#Matriz de documentos
dtm <- as.DocumentTermMatrix(tdm)


#MODELADO DE TEMAS: Latent Dirichlet Allocation (LDA)
doc.lengths <- apply(dtm , 1, sum)
dtm <- dtm[doc.lengths >0,]
iter <- 30000
#seed <-sample(1:100000, 1)

#2 temas
k <- 2
lda <- LDA(dtm, k = k, method="Gibbs", control=list(iter = iter)) 
#obtengo los 10 terminos mas importantes de cada grupo 
lda.terms <- as.matrix(terms(lda,10))
lda.terms

#3 temas
k <- 3
lda_3 <- LDA(dtm, k = k, method="Gibbs", control=list(iter = iter)) 
lda_3.terms <- as.matrix(terms(lda_3,10))
lda_3.terms

#4 temas
k <- 4
lda_4 <- LDA(dtm, k = k, method="Gibbs", control=list(iter = iter)) 
lda_4.terms <- as.matrix(terms(lda_4,10))
lda_4.terms

#5 temas
k <- 5
lda_5 <- LDA(dtm, k = k, method="Gibbs", control=list(iter = iter)) 
lda_5.terms <- as.matrix(terms(lda_5,10))
lda_5.terms
#nombres de los temas (con 5 terminos mas importantes de cada grupo)
#top5termsPerTopic <- terms(lda, 5)
#topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
#topicNames

