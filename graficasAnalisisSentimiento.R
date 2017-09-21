#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"

company="@Cruzcampo"


require(mongolite)
require(jsonlite)
require(plyr)
require(dplyr)
require(ggplot2)
#Se crea el objeto mongolite para la conexión con la base de datos y la colección tweets
db<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
#recupera los tweets de una empresa
Tweets<- db$find(query =   paste0('{"company":"',company,'"}'), fields = '{"_id" : 0}')
Tweets<-unique.data.frame(Tweets)

#función que chequea si se produce error al pasar a minusculas el texto pasado por parametro
catch.error = function(x)
{
  y = NA
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # En caso de no haber error lo pasa a minusculas sino devolveria NA
  if (!inherits(catch_error, "error")){
    y = tolower(x)
  }
  return(y)
}

#función que trata el texto de los tweets y los limpia de información no necesaria como url, nicknames, el nombre de la compañia,etc.
trataTextoTweets<- function(tweet){
  #transforma el formato del texto de UTF8 a latin1 
  tweet <- sapply(tweet,function(row) iconv(row,"latin1","UTF-8",sub=""))
  # Se pasa el texto a minusculas 
  tweet = catch.error(tweet)
  tweet = gsub("ã¡", "a", tweet)
  tweet = gsub("ã©", "e", tweet)
  tweet = gsub("ã³", "o", tweet)
  tweet = gsub("ãº", "u", tweet)
  tweet = gsub("ã±", "ñ", tweet)
  tweet = gsub("ã¨", "e", tweet)
  tweet = gsub("ã²", "o", tweet)
  tweet = gsub("ã", "i", tweet)
  tweet =gsub("http[^[:space:]]*", "", tweet)
  tweet =gsub("@[^[:space:]]*", "", tweet)
  tweet =gsub( gsub("@", "", tolower(company)), "", tweet)
  # Se limpian los tweets de enlaces, de entradas de retweets, de hashtags y de signos de puntuación para el análisis de sentimiento
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("@\\w+", " ", tweet)
  tweet = gsub("[[:punct:]]", " ", tweet)
  tweet = gsub("[[:digit:]]", " ", tweet)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  return(tweet)
}

tratarTweets<- function(Tweets) {
  TweetsTratados = Tweets
  TweetsTratados$text=sapply(Tweets$text, trataTextoTweets)
  # Se eliminan los registros que tienen el texto del tweet igual a NA
  TweetsTratados = TweetsTratados[which(!is.na(TweetsTratados$text)),]
  # Se eliminan los registro repetidos
  TweetsTratados = unique(TweetsTratados)
  return(TweetsTratados)
  
}


TweetsTratados = tratarTweets(Tweets)



Result<-TweetsTratados[which(!is.na(TweetsTratados$score1)),]

Result$score1<-as.numeric(Result$score1)
Result<-Result[which(!is.na(Result$score1)),]
ResultFreq =as.data.frame(table(as.numeric(Result$score1)))
names(ResultFreq)[1]="score"
 
par(bg = "white", mar = c(10,5,5,5),mfrow=c(1,1))
barplot(ResultFreq$Freq, las = 2, names.arg= ResultFreq$score,  main = paste0('Histograma puntuación de ',company))

mean(Result$score1)
sd(Result$score1)



df<-data.frame()
for (e in TweetsTratados$class_emotion){
  y<-as.data.frame(e)
  if(is.null(y$BEST_FIT)){
    y$BEST_FIT<-NA
  }
  
  df<-rbind(df,y)
}
Emotion = df[,7]
Emotion[is.na(Emotion)] = "NA"
Emotion[which(Emotion=="disgust")]= "disgusto"
Emotion[which(Emotion=="joy")]= "alegria"
Emotion[which(Emotion=="sadness")]= "tristeza"
Emotion[which(Emotion=="anger")]= "enfado"
Emotion[which(Emotion=="fear")]= "miedo"
Emotion[which(Emotion=="surprise")]= "sorpresa"
Emotion[which(Emotion=="disgust")]= "disgusto"



df2<-data.frame()
for (e in TweetsTratados$class_polarity){
  y<-as.data.frame(e)
  if(is.null(y$BEST_FIT)){
    y$BEST_FIT<-NA
  }
  
  df2<-rbind(df2,y)
}
TweetsPol = df2[,4]
TweetsPol[which(TweetsPol=="positive")]= "positivo"
TweetsPol[which(TweetsPol=="negative")]= "negativo"

SentimentDataFrame = data.frame(text=TweetsTratados$text, emoción=Emotion, polaridad=TweetsPol, stringsAsFactors=FALSE)
SentimentDataFrame = within(SentimentDataFrame, emoción <- factor(emoción, levels=names(sort(table(emoción), decreasing=TRUE))))


plotSentiments1<- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emoción)) +
    geom_bar(aes(y=..count.., fill=emoción)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Número de Tweets') +
    xlab('Emociones')
}
plotSentiments1(SentimentDataFrame, paste('Análisis de sentimientos de los Tweets en Twitter de',company))


plotSentiments2 <- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polaridad)) +
    geom_bar(aes(y=..count.., fill=polaridad)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Polaridad')
}
plotSentiments2(SentimentDataFrame, paste('Análisis de polaridad de los Tweets en Twitter de',company) )





df3<-data.frame()

for (e in TweetsTratados$class_polarity){
  y<-as.data.frame(e)
  if(is.null(y$BEST_FIT)){
    y$BEST_FIT<-NA
  }
  
  df3<-rbind(df3,y)
  
}
df3$created<-TweetsTratados$created
df3$created <- as.Date(df3$created)
df3$tweet<-df3$BEST_FIT

detach("package:plyr", unload=TRUE) 
by.tweet <- group_by(df3, tweet, created)
by.tweet <- summarise(by.tweet, number=n())


ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  ggtitle(company)



#GENERACIÓN DE LAS GRAFICAS DE USUARIOS CON MAS TWEETS POSITIVOS / NEGATIVOS


#Se crea el objeto mongolite para la conexión con la base de datos y la colección tweets
db<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
#recupera los tweets de una empresa
Tweets<- db$find(query =   paste0('{"company":"',company,'"}'), fields = '{"_id" : 0}')


df<-data.frame()
for (e in Tweets$class_emotion){
  y<-as.data.frame(e)
  if(is.null(y$BEST_FIT)){
    y$BEST_FIT<-NA
  }
  
  df<-rbind(df,y)
}
Emotion = df[,7]
Emotion[is.na(Emotion)] = "NA"
Emotion[which(Emotion=="disgust")]= "disgusto"
Emotion[which(Emotion=="joy")]= "alegria"
Emotion[which(Emotion=="sadness")]= "tristeza"
Emotion[which(Emotion=="anger")]= "enfado"
Emotion[which(Emotion=="fear")]= "miedo"
Emotion[which(Emotion=="surprise")]= "sorpresa"
Emotion[which(Emotion=="disgust")]= "disgusto"



df2<-data.frame()
for (e in Tweets$class_polarity){
  y<-as.data.frame(e)
  if(is.null(y$BEST_FIT)){
    y$BEST_FIT<-NA
  }
  
  df2<-rbind(df2,y)
}
TweetsPol = df2[,4]
TweetsPol[which(TweetsPol=="positive")]= "positivo"
TweetsPol[which(TweetsPol=="negative")]= "negativo"


dfusempo<-data.frame( emoción=Emotion, polaridad=TweetsPol, stringsAsFactors=FALSE)
screenName<- db$find(query =   paste0('{"company":"',company,'"}'), fields = '{"_id" : 0,"screenName":1}')
names(screenName)
dfusempo$screenName<-as.character(screenName$screenName)

typeof(dfusempo$emoción)

detach("package:plyr", unload=TRUE) 
by.usempo <- group_by(dfusempo, screenName, polaridad)
by.usempo <- summarise(by.usempo, number=n())


top_usempo <- arrange(by.usempo,desc(number))

top_usepos<-top_usempo[which(top_usempo$screenName!=gsub("@","",company)),]
top_usepos<-top_usepos[which(top_usepos$polaridad=="positivo"),]
top_usepos<-top_usepos[1:10,]

top_useneg<-top_usempo[which(top_usempo$screenName!=gsub("@","",company)),]
top_useneg<-top_useneg[which(top_useneg$polaridad=="negativo"),]
top_useneg<-top_useneg[1:10,]



plotSentiments3 <- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(data=sentiment_dataframe, aes(x=screenName,y=number)) +
    geom_bar(position="dodge",stat="identity") + 
    scale_fill_brewer(palette="RdGy") +
    coord_flip() +
    ggtitle(title) +
    
    theme(legend.position='right') + ylab('Número de tweets') +
    xlab('Usuarios')
}

plotSentiments3(top_usepos, paste('Usuarios con más Tweets positivos sobre',company) )


plotSentiments3(top_useneg, paste('Usuarios con más Tweets negativo sobre',company) )
