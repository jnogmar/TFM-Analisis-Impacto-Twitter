#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"
#"@AyesaNews"
geoLocalizaciónUsuarios <- function (company,n){
  #Comprobación previa del parámetro de entrada
  if (is.na(company)){
    return (NULL)
  } 
  require(RCurl)
  require(mongolite)
  require(jsonlite)
  #Se crean los objetos mongo con la conexión a la base de datos para las colecciones followers y friends respectivamente
  dbfollowers<-mongo(collection = "followers", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  dbfriends<-mongo(collection = "friends", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  
  #se obtienen de la base de datos todos los usuarios followers y los friends de la compañia indicada que aún no han sido geo localizados
  Followers<- dbfollowers$find(query =paste0('{"company":"',company,'","geometry_location":{"$exists":false},"numtweets":{"$gt":0}}'), fields = '{"_id" : 0}')
  #Se excluyen los usuarios que no han indicado su localización
  Followers <- Followers[which(!(Followers$location=="")),]
  
  Friends<- dbfriends$find(query =paste0('{"company":"',company,'","geometry_location":{"$exists":false},"numtweets":{"$gt":0}}'), fields = '{"_id" : 0}')
  #Se excluyen los usuarios que no han indicado su localización
  Friends <- Friends[which(!(Friends$location=="")),]
  
  
  dbtweets<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  
  #Se indica el API key del API de Google
  YOUR_API_KEY="---------------------------------------"
  geoCode <- function(x,y){
      #Se monta la url de la petición
      u <- paste('https://maps.googleapis.com/maps/api/geocode/json?address=', getElement(x, "location"), "&key=", YOUR_API_KEY, sep = "")
      u <- URLencode(u)
      #Se realiza la petición
      doc <- getURL(u)
      #Se crea un objeto jsonlite con la respuesta obtenida
      r <- fromJSON (doc)
      if(r$status=="OK") {
        #En caso de que la respuesta tenga un estado OK se registran los datos de latitud y longitud 
        #para actualizar los datos del usuario con su geo-localización
        lat <- r$results$geometry$location$lat[[1]] 
        lng <- r$results$geometry$location$lng[[1]] 
        location_type  <- r$results$geometry$location_type[[1]] 
        formatted_address  <-r$results$formatted_address [[1]] 
        location = c(lat, lng, location_type, formatted_address)
        #Sys.sleep(0.5)
      } else {
        #En otro caso se crea una localización vacia
        print(r$status)
        lat <- NA
        lng <- NA
        location_type  <- NA 
        formatted_address <-NA
        location = c(NA,NA,NA, NA)
      }
      #Se actualiza el usuario
      if (y=="followers"){
        dbfollowers$update(query=paste0('{"id":"',getElement(x, "id"),'","company":"',getElement(x, "company"),'"}'), update = paste0('{"$set":{"geometry_location":[{"lat":"',lat,'","lng":"',lng,'","location_type":"',location_type,'","formatted_address":"',formatted_address,'"}]}}'), upsert = FALSE, multiple = FALSE)
      }
      if (y=="friends"){
        dbfriends$update(query=paste0('{"id":"',getElement(x, "id"),'","company":"',getElement(x, "company"),'"}'), update = paste0('{"$set":{"geometry_location":[{"lat":"',lat,'","lng":"',lng,'","location_type":"',location_type,'","formatted_address":"',formatted_address,'"}]}}'), upsert = FALSE, multiple = FALSE)
      }
    
  }
  if(nrow(Followers)>0){
    apply(Followers[1:1250,], 1, geoCode,... = "followers")
  }
  if(nrow(Friends)>0){
    apply(Friends[1:1,], 1, geoCode,"friends")
  }
}