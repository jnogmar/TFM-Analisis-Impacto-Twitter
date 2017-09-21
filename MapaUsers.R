# citation('ggmap')
mapaUsers<-function(company){
  #Comprobación previa del parámetro de entrada
  if (is.na(company)){
    return (NULL)
  } 
  require("ggmap")
  require("ggplot2")
  require("plyr")
  
  dbfollowers<-mongo(collection = "followers", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  dbfriends<-mongo(collection = "friends", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  
  #Se obtienen los followers y friends geo-posicionados de la compañía indicada 
  FollowersGeo<- dbfollowers$find(query =paste0('{"company":"',company,'","geometry_location":{"$exists":true}}'), fields = '{"_id" : 0}')
  FriendsGeo<- dbfriends$find(query =paste0('{"company":"',company,'","geometry_location":{"$exists":true}}'), fields = '{"_id" : 0}')
  #Se unen los dataframes
  Users <- rbind(FollowersGeo,FriendsGeo)
  #Se aplica una función que devuelve una lista con un conjunto que incluye la latitud y longitud de cada usuario
  ugeo<-ldply(Users$geometry_location,function(x){lat=as.numeric(x[1]); lng=as.numeric(x[2]); return(c(lat, lng));}) 
  names(ugeo)  <- c("lat","lng")
  #Se crea el data frame con el conteo de las longitudes latitudes al que se le quitan los valores NA
  df <- count(ugeo, c("lat","lng"))
  df <- na.omit(df)
  #Se crea el objeto mapa centrado en Madrid
  map <- get_map(location=c(lon=-3.7025600,lat= 40.4165000), zoom=6, maptype="toner")
  #se crea la gráfica con el mapa donde se indica que los ejes son la longitud y latitud y se le pasan como datos el conteo de las longitudes latitudes
  finalmap <- ggmap(map, base_layer = ggplot(aes(x=lng, y=lat), data = df))
  #se añade la estadistica de densidad y se le indica la transición y escala de colores, de gris a rojo(de menor a mayor)
  finalmap + stat_density2d(aes(x = lng, y = lat, fill = ..level.., alpha = ..level.., size=freq), bins = 9, geom = "polygon",alpha=0.2, data = df) + scale_fill_gradient(trans = "sqrt", low = "gray5", high = "red") 
}


#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"
company = "@Mercadona"
mapaUsers(company)
