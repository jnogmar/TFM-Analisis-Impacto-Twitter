obtenerFollowersYFriends <- function (company){
  if (is.na(company)){
    return (NULL)
  }
  require(httr)
  require(mongolite)
  
  #Se carga la configuración de autenticación en el API de Twitter
  source('~/Máster Universitario Visual Analytics and Big Data/TFM/Source/GIHUB/autenticarAPITwitter.R')
  
  #Se crean los objetos mongo con la conexión a la base de datos para las colecciones followers y friends respectivamente
  dbfollowers<-mongo(collection = "followers", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  dbfriends<-mongo(collection = "friends", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  
  #se obtiene el usuario de twitter
  user = getUser(company)
  
  #se obtienen todos los IDs de los seguidores del usuario de twitter
  folowersID = user$getFollowerIDs(retryOnRateLimit=9999999)
  #se calcula el número de veces que se ha de realizar el bucle para recopilar cada vez 50000 followers
  #con esta forma de reccorrer los IDs de una cuenta con un número grande de seguidores, se obtienen buenos resultados y se salvan las restricciones del API 
  ntimes = ceiling(length(folowersID)/50000)
  
  for(i in c(1:ntimes)){
    from = (50000*(i-1))+1
    to = (50000*i)
    to = min(to,length(folowersID))
    #Se obtienen todos los datos de los usuarios con el ID del follwer
    folowers<-lookupUsers(folowersID[from:to])
    followers_df = twListToDF(folowers)
    #Se incluye la empresa de la que es seguidor y se tratan los datos de texto para indicarles un formato que acepta la función insertar de mongolite
    followers_df$company<-company
    followers_df$description <- sapply(followers_df$description,function(row) iconv(row,"UTF-8","latin1",sub=""))
    followers_df$url <- sapply(followers_df$url,function(row) iconv(row,"UTF-8","latin1",sub=""))
    followers_df$name <- sapply(followers_df$name,function(row) iconv(row,"UTF-8","latin1",sub=""))
    followers_df$profileImageUrl <- sapply(followers_df$profileImageUrl,function(row) iconv(row,"UTF-8","latin1",sub=""))
    followers_df$location <- sapply(followers_df$location,function(row) iconv(row,"UTF-8","latin1",sub=""))
    #Se insertan todos los datos de los seguidores recuperados
    dbfollowers$insert(data =  followers_df)
  }
  #se obtienen todos los IDs de los usuarios a los que la empresa sigue y ellos siguen a la empresa, es decir son amigos.
  friendsID = user$getFriendIDs (retryOnRateLimit=9999999 )
  ntimes = ceiling(length(friendsID)/50000)
  for(i in c(1:ntimes)){
    from = (50000*(i-1))+1
    to = (50000*i)
    to = min(to,length(friendsID))
    #Se obtienen todos los datos de los usuarios con el ID del friend
    friends<-lookupUsers(friendsID[from:to])
    friends_df = twListToDF(friends)
    #Se incluye la empresa de la que es amigo y se tratan los datos de texto para indicarles un formato que acepta la función insertar de mongolite
    friends_df$company<-company
    friends_df$description <- sapply(friends_df$description,function(row) iconv(row,"UTF-8","latin1",sub=""))
    friends_df$url <- sapply(friends_df$url,function(row) iconv(row,"UTF-8","latin1",sub=""))
    friends_df$name <- sapply(friends_df$name,function(row) iconv(row,"UTF-8","latin1",sub=""))
    friends_df$profileImageUrl <- sapply(friends_df$profileImageUrl,function(row) iconv(row,"UTF-8","latin1",sub=""))
    friends_df$location <- sapply(friends_df$location,function(row) iconv(row,"UTF-8","latin1",sub=""))
    #Se insertan todos los datos de los amigos recuperados
    dbfriends$insert(data =  friends_df)
  }
  #se borra y cierra la conexión con la base de datos 
  rm(dbfollowers)
  rm(dbfriends)
  gc() 
}

