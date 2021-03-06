sentAnalisis<-function(company){
  
  require(mongolite)
  Sys.setlocale("LC_CTYPE", "spanish")
  
  #Se crea el objeto mongolite para la conexi�n con la base de datos y la colecci�n tweets
  db<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  #recupera los tweets de una empresa
  Tweets<- db$find(query =   paste0('{"company":"',company,'"}'), fields = '{"_id" : 0}')

  #funci�n que chequea si se produce error al pasar a minusculas el texto pasado por parametro
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
  
  #funci�n que trata el texto de los tweets y los limpia de informaci�n no necesaria como url, nicknames, el nombre de la compa�ia,etc.
  trataTextoTweets<- function(tweet){
    #transforma el formato del texto de UTF8 a latin1 
    tweet <- sapply(tweet,function(row) iconv(row,"latin1","UTF-8",sub=""))
    
    # Se pasa el texto a minusculas 
    tweet = catch.error(tweet)
    
    tweet = gsub("�", "a", tweet)
    tweet = gsub("�", "e", tweet)
    tweet = gsub("�", "o", tweet)
    tweet = gsub("�", "u", tweet)
    tweet = gsub("�", "�", tweet)
    tweet = gsub("�", "e", tweet)
    tweet = gsub("�", "o", tweet)
    tweet = gsub("�", "i", tweet)
    tweet =gsub("http[^[:space:]]*", "", tweet)
    tweet =gsub("@[^[:space:]]*", "", tweet)
    tweet =gsub( gsub("@", "", tolower(company)), "", tweet)

    # Se limpian los tweets de enlaces, de entradas de retweets, de hashtags y de signos de puntuaci�n para el an�lisis de sentimiento
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
  
  opinion.lexicon.pos = scan("D:/Documentos/M�ster Universitario Visual Analytics and Big Data/TFM/DW/isol/positivas_mejorada.csv",sep = "",
                             what='character', comment.char=';')
  opinion.lexicon.neg =
    scan("D:/Documentos/M�ster Universitario Visual Analytics and Big Data/TFM/DW/isol/negativas_mejorada.csv",sep = "",
         what='character', comment.char=';')
  
  
  words.positive = opinion.lexicon.pos
  words.negative = opinion.lexicon.neg
  #funci�n que calcula la puntuaci�n seg�n las palabras negativas y positivas que tienen el texto del tweet
  puntuacionSentimientos = function(sentences, words.positive, words.negative)
  {
    require(plyr)
    require(stringr)
    
    puntuaciones<- function(sentence, words.positive, words.negative) {
      
      # Se limpian los tweets de digitos, de signos de puntuaci�n 
      text = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', getElement(sentence, "text") )))
      # Se pasa el texto a minusculas 
      text = tolower(text)
      
      # Se separa el texto por los espacios 
      words = unlist(str_split(text, '\\s+'))
      
      # se obtienen las coincidencias de las palabras con las palabras de los listados de palabras positivas y negativas
      pos.matches = !is.na(match(words, words.positive))
      neg.matches = !is.na(match(words, words.negative))
      
      # se calcula la puntcuaci�n como la resta entre las coincidencias de palabras positivas menos las coincidencias de palabras negativas
      puntuacion = sum(pos.matches) - sum(neg.matches)
     
      db$update(query=paste0('{"id":"',getElement(sentence, "id"),'","company":"',getElement(sentence, "company"),'"}'), 
                update = paste0('{"$set":{"score1":"', puntuacion,'"}}'), upsert = FALSE, multiple = FALSE)
    }
    
    apply(sentences,1,puntuaciones, words.positive=words.positive, words.negative=words.negative)
    
  }
  
  #Se generan las puntuaciones y se almacenan
  puntuacionSentimientos(TweetsTratados, words.positive , words.negative)

  
  devtools::install_github("mjockers/syuzhet")
  nrc_sentiment<-function(x){
    require (syuzhet)
    
    d=get_nrc_sentiment(char_v = getElement(x, "text"), language = "spanish")
    
    db$update(query=paste0('{"id":"',getElement(x, "id"),'","company":"',getElement(x, "company"),'"}'), 
              update = paste0('{"$set":{"nrc_sentiment":', toJSON(d),'}}'), upsert = FALSE, multiple = FALSE)
    
    
    
  }
  
  #Se generan el analisis nrc de sentimiento y se almacena 
  apply(TweetsTratados,1,nrc_sentiment )
  
  
  #install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
  #require(Rstem)
  #require(devtools)
  #install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz") 
  #require(sentiment)
  #ls("package:sentiment")
  #library(sentiment)
  
  emotion <- function(x){
    
    e = classify_emotion(getElement(x,"text"), algorithm="bayes",verbose = FALSE, prior=1.0)
    
    db$update(query=paste0('{"id":"',getElement(x, "id"),'","company":"',getElement(x, "company"),'"}'), 
              update = paste0('{"$set":{"class_emotion":', toJSON(as.data.frame(e)),'}}'), upsert = FALSE, multiple = FALSE)
  }
  #Se genera el analisis de sentimiento para los tweets aplicando el metodo naive-bayes
  apply(TweetsTratados,1,emotion) 
  

  polarity <- function(x){
    e = classify_polarity(getElement(x,"text"), algorithm="bayes")
    db$update(query=paste0('{"id":"',getElement(x, "id"),'","company":"',getElement(x, "company"),'"}'), 
              update = paste0('{"$set":{"class_polarity":', toJSON(as.data.frame(e)),'}}'), upsert = FALSE, multiple = FALSE)
    
  }
  #Se genera el analisis de polaridad (negativo,positivo) para los tweets aplicando el metodo naive-bayes
  apply(TweetsTratados,1,polarity)
  
}