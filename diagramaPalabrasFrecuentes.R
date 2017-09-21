
diagramaPalabrasFrecuentes <- function (company){  
  require(mongolite)
  require(rJava)
  require(rjson)
  Sys.setlocale("LC_CTYPE", "spanish")
 
  #Se crea el objeto mongolite para la conexión con la base de datos y la colección tweets
  db<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
  #recupera los tweets de una empresa
  outputtw.df<- db$find(query =   paste0('{"company":"',company,'"}'), fields = '{"_id" : 0}')
  
  require(tm)
  
  #transforma el formato del texto de UTF8 a latin1 
  outputtw.df$text <- sapply(outputtw.df$text,function(row) iconv(row,"latin1","UTF-8",sub=""))
  #transforma el texto a minusculas
  outputtw.df$text = tolower(outputtw.df$text)
  #transforma los caracteres especiales como vocales con tilde el texto
  outputtw.df$text = gsub("ã¡", "a", outputtw.df$text)
  outputtw.df$text = gsub("ã©", "e", outputtw.df$text)
  outputtw.df$text = gsub("ã³", "o", outputtw.df$text)
  outputtw.df$text = gsub("Ã³", "o", outputtw.df$text)
  outputtw.df$text = gsub("ãº", "u", outputtw.df$text)
  outputtw.df$text = gsub("ã±", "ñ", outputtw.df$text)
  outputtw.df$text = gsub("ã¨", "e", outputtw.df$text)
  outputtw.df$text = gsub("ã²", "o", outputtw.df$text)
  outputtw.df$text = gsub("ã", "i", outputtw.df$text)
  
  #Se obtiene el corpues del vector con todo los tweets de una empresa 
  corpustt=Corpus(VectorSource(outputtw.df$text), readerControl = list(reader = readPlain,
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
  corpustt = tm_map(corpustt, stripWhitespace)
  corpustt=tm_map(corpustt, removeWords, c("hola","saludos","adios","por favor","gracias","saludo","ahora","hoy","ayer","the","telefÃ","nica","tambien","dia","telepizza"))
  
  #se genera la matriz de terminos de los codumentos 
  matrix = TermDocumentMatrix(corpustt)
  #se genera el dataframe con la suma de la frecuencia en la que aparece un termino en todos los documentos
  term.freq <- rowSums(as.matrix(matrix))
  term.freq <- subset(term.freq, term.freq >= 200)
  dfTerm <- data.frame(term = names(term.freq), freq = term.freq)
  
  require(ggplot2)
  #se genera el diagrama de barras con las palabras y la frecuencia
  ggplot(dfTerm, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Palabras") + ylab("Frecuencia") + coord_flip()  + theme(axis.text=element_text(size=8))
}

#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"
company = "@telepizza_es"

diagramaPalabrasFrecuentes(company)

