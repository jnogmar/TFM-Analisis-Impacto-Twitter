almacenaPalabrasFrecuentes <- function (company){
  
  require(mongolite)
  require(rJava)
  require(rjson)
  require(tm)
  Sys.setlocale("LC_CTYPE", "spanish")
  
  #Se crean los objetos mongolite para la conexi´´on conla base de datos y las colecciones tweets y frequentwords
  dbtweet<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  dbfreqword<-mongo(collection = "frequentwords", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  
  #Se obtienen las disitas fechas en las que se dispone de tweets almacenados
  outputtw.df<- dbtweet$distinct("yearMonthDayUTC",query =   paste0('{"company":"',company,'"}'))
  outputtw.df<-as.data.frame(outputtw.df)
  names(outputtw.df)<-c("Fecha")
  fechas<-as.data.frame(outputtw.df[order(outputtw.df$Fecha),])
  names(fechas)<-c("Fecha")
  #función que trata el texto de los tweets de una fecha, obtiene la matriz de fecuencia y almacena la frecuencia por día de las palabras 
  frequents<-function(x)
  {
    fecha = getElement(x,"Fecha")
    #recupera los tweets de una fecha almacenados para una empresa, 
    tweets<- dbtweet$find(query =   paste0('{"company":"',company,'", "yearMonthDayUTC":"',fecha,'"}'))
    #transforma el formato del texto de UTF8 a latin1 
    tweets$text <- sapply(tweets$text,function(row) iconv(row,"latin1","UTF-8",sub=""))
    #transforma el texto a minusculas
    tweets$text = tolower(tweets$text)
    #transforma los caracteres especiales como vocales con tilde el texto
    tweets$text = gsub("ã¡", "a", tweets$text)
    tweets$text = gsub("ã©", "e", tweets$text)
    tweets$text = gsub("ã³", "o", tweets$text)
    tweets$text = gsub("ãº", "u", tweets$text)
    tweets$text = gsub("ã±", "ñ", tweets$text)
    tweets$text = gsub("ã¨", "e", tweets$text)
    tweets$text = gsub("ã²", "o", tweets$text)
    tweets$text = gsub("ã", "i", tweets$text)
    
    #Se obtiene el corpues del vector con todo los tweets de una empresa en una fecha
    corpustt=Corpus(VectorSource(tweets$text), readerControl = list(reader = readPlain,
                                                                         language = "es",
                                                                         load = TRUE) )
    corpustt = tm_map(corpustt, content_transformer(tolower))
    #funciones para eliminar las urls, los nicknames de los usuarios y el nombre de la compañia
    removeURL = function(x) gsub("http[^[:space:]]*", "", x)
    removenicknames = function(x) gsub("@[^[:space:]]*", "", x)
    removecompany = function(x) gsub( gsub("@", "", tolower(company)), "", x)
   
    #Se trata el contenido del corpus eliminando información no necesaria como puntiación, números, palabras como articulos, preposiciones y palabras protocolarias
    corpustt = tm_map(corpustt, removecompany)  
    corpustt = tm_map(corpustt, removeURL)
    corpustt = tm_map(corpustt, removenicknames)
    corpustt=tm_map(corpustt, removePunctuation)
    corpustt=tm_map(corpustt, removeNumbers)
    corpustt=tm_map(corpustt, removeWords, stopwords("spanish"))
    corpustt=tm_map(corpustt, removeWords, c("hola","saludos","adios","por favor","gracias","saludo","ahora","hoy","ayer","telepizza","tambien","dia"))
    corpustt = tm_map(corpustt, stripWhitespace)
    
    #se genera la matriz de terminos de los codumentos 
    matrix = TermDocumentMatrix(corpustt)
    
    which(apply(matrix, 1, sum)>1)
    #se genera el dataframe con la suma de la frecuencia en la que aparece un termino en todos los documentos
    term.freq <- rowSums(as.matrix(matrix))
    term.freq <- subset(term.freq, term.freq >= 1)
    dfTerm <- data.frame(term = names(term.freq), freq = term.freq)
    
    #Se registran los terminos y frecuencias en la colección de palabras frecuentes por fecha para una compañia
    if(nrow(dfTerm)>0){
      dfTerm$Fecha<-fecha
      dfTerm$company<-company
      dbfreqword$insert(dfTerm)
    }
  }
  #se aplica la función a todas las fechas distintas obtenidas
  apply(fechas,1,frequents)
  
}

#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"
company = "@telepizza_es"

companies<- c("@Cruzcampo",  "@elcorteingles",  "@Iberia",  "@Renfe",  "@Telefonica",  "@telepizza_es",  "@Mercadona")
for(company in companies){
  
  almacenaPalabrasFrecuentes(company) 
}
#almacenaPalabrasFrecuentes("@Mercadona")
