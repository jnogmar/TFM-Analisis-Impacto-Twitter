obtenerTweets <- function ( twitterUsers , wordssearch, n, datesince=NaN ){
  
  #comprobaci�n de los par�metros de entrada
  if (length(twitterUsers)<=0){
    return (NULL)
  }
  if (length(wordssearch)<=0){
    return (NULL)
  }
  if (is.na(n)|| n<=0){
    return (NULL)
  }
  #Se carga la configuraci�n de autenticaci�n en el API de Twitter
  source('~/M�ster Universitario Visual Analytics and Big Data/TFM/Source/GIHUB/autenticarAPITwitter.R')
  
  ind  = 1
  require(twitteR)
  require(httpuv)
  
  #Se recorren los usuarios de twitter del par�metro de entrada para obtener sus tweets 
  #y los que est�n asociados a la palabara a buscar asocidado a dicho usuario. 
  for (tu in twitterUsers){
    searchterm = tu
    searchword = wordssearch[ind]
    ind  = ind +1
    #se obtiene el usuario de twitter
    user = getUser(searchterm)
    
    #se obtienen los n tweets del usuario incluyendo retweets y respuestas
    usertweets = userTimeline(user, n, includeRts = T,excludeReplies = F)
    #se almacenan los tweets en un dataframe
    dfusertweets <- twListToDF(usertweets)
    dfusertweets<-dfusertweets[, order(names(dfusertweets))]

    #se obtienen los n tweets que s�n tendencia relacionados con el usuario.
    #En caso de indicar una fecha se obtendran los n tweets que s�n tendencia relacionados el usuario desde la fecha indicada.
    if(is.nan(datesince)){
      trendingTweets = searchTwitter(searchterm, n=n, lang = "es")
    }
    else{
      trendingTweets = searchTwitter(searchterm, since = datesince,n=n, lang = "es")
    }
    
    #Si el numero de tweets es cero se crea un dataframe vac�o para que cuando vayan a tratarse estos twwets no haga nada.
    #En caso de obtener un n�mero mayor a cero se crea un dataframe con los tweets 
    if(length(trendingTweets)==0){
      dftrendingTweets=data.frame()
      
    }else{
      dftrendingTweets<-twListToDF(trendingTweets)
      dftrendingTweets<-dftrendingTweets[, order(names(dftrendingTweets))]

    }
    #se obtienen los n tweets relacionados con la palabra de busqueda asociada al usuario.
    #En caso de indicar una fecha se obtendran los n tweets relacionados con la palabra de busqueda asociada al usuario desde la fecha indicada.
    if(is.nan(datesince)){
      searchwordTweets = searchTwitter(searchword, n=n, lang = "es")
    }
    else{
      searchwordTweets = searchTwitter(searchword, since = datesince,n=n, lang = "es")
    }
    #Si el numero de tweets es cero se crea un dataframe vac�o para que cuando vayan a tratarse estos twwets no haga nada.
    #En caso de obtener un n�mero mayor a cero se crea un dataframe con los tweets
    if(length(searchwordTweets)==0){
      dfsearchwordTweets=data.frame()
      
    }else{
      dfsearchwordTweets<-twListToDF(searchwordTweets)
      dfsearchwordTweets<-dfsearchwordTweets[, order(names(dfsearchwordTweets))]
      
    }
    require(mongolite)
    require(rJava)
    require(jsonlite)
    
    #Se crea el objeto mongo con la conexi�n a la base de datos 
    dbtweets<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
    #funci�n encargada de insertar los tweets en la base de datos en el caso de que est�n ya almacenados 
    insertarTweets <- function(row){
      row$company<-searchterm
      row$text <- sapply(row$text,function(row) iconv(row,"UTF-8","latin1",sub=""))
      #comprobaci�n de la existencia en la base de datos del tweet
      output.df <- dbtweets$find(paste0('{"company":"',row$company,'","id":"',row$id,'"}'))
     
      if(nrow(output.df)==0){
        row_json <- toJSON(row,auto_unbox = TRUE)
        #insercci�n del registro
        dbtweets$insert(data =  row_json)
        
      }
    
    }
    
    #Se aplica la insercci�n de tweets a los conjuntos de tweets recuperados
    if(nrow(dfusertweets)>0){
      apply(dfusertweets, 1, insertarTweets)
    }
    if(nrow(dftrendingTweets)>0){
      apply(dftrendingTweets, 1, insertarTweets)
    }
    if(nrow(dfsearchwordTweets)>0){
      apply(dfsearchwordTweets, 1, insertarTweets)
    }
    #se borra y cierra la conexi�n con la base de datos 
    rm(dbtweets)
    gc() 
  }
}