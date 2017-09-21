hashtagsFrecuentes<-function(company){
  
  #Se crea el objeto mongolite para la conexión con la base de datos y la colección tweets
  dbtweets<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  #recupera los tweets de una empresa
  outputtw.df<- dbtweets$find(query =   paste0('{"$and":[{"company":"',company,'"},{"screenName":{"$ne":"',gsub("@","",company),'"}}]}'), fields = '{"_id" : 0}')
  #transforma el formato del texto de UTF8 a latin1 
  textTweets <- sapply(outputtw.df$text,function(row) iconv(row,"latin1","UTF-8",sub=""))
  
  extract.hashtags = function(text){
    #se buscan las coincidencias con la expresión regular que detecta un hashtag
    hashtag.pattern = "#[[:alpha:]]+"
    have.hashtag = grep(x = text, pattern = hashtag.pattern)
    
    hashtag.matches = gregexpr(pattern = hashtag.pattern,
                            text = text[have.hashtag])
    extracted.hashtag = regmatches(x = text[have.hashtag], m = hashtag.matches)
    #se genera y devuelve un dataframe ordenado con los hashtags encontrados y la frecuencia con que aparecen
    df = data.frame(table(tolower(unlist(extracted.hashtag))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  #se obtienen los 50 primeros hashtags
  dat = head(extract.hashtags(textTweets),50)
  dat2 = transform(dat,tag = reorder(tag,freq))
  

  require(ggplot2)
  
  #se genera el diagrama de barras con las palabras y la frecuencia
  ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity") + xlab("Hashtags") + ylab("Frecuencia") + coord_flip()  + labs(title = paste("Frecuencia de Hashtag en los tweets de",company)) + theme(axis.text=element_text(size=8))
 
}

#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"
company = "@Mercadona"
hashtagsFrecuentes(company)
