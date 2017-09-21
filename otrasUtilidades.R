
#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"

####################################################################################################################
################# Top 3 de las palabras más frecuentes por fecha de los tweets relacionados con una compañia 
####################################################################################################################

require (plyr)
require (dplyr)

require (mongolite)
company="@Mercadona"
dbfreqword<-mongo(collection = "frequentwords", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)

outputtw.df<- dbfreqword$find(query =   paste0('{"company":"',company,'"}'),fields = '{"_id" : 0,"term":1,"freq":1   ,   "Fecha" :1}')

d1<-arrange(outputtw.df, Fecha,desc(freq))

d2 <- d1[which(d1$freq>20),]

fechas <-  d1$Fecha
fechas<-unique.default(fechas)

require (plyr)
dresult <- ldply(fechas, function(x){ return(filter(d2, Fecha == x)[1:3,]);})

dresult <- dresult[which(!is.na(dresult$term)),]

dresult

#install.packages("gridExtra")
#library("gridExtra")

#grid.table(dresult)

print(tbl_df(dresult), n=100)

####################################################################################################################
################# Dispositivos utilizados por los usuarios que interactuan con las compañias
####################################################################################################################

dbtweet<-mongo(collection = "tweets", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
outputtw.df<- dbtweet$find(query =   paste0('{"company":"',company,'","screenName":{"$ne":"',gsub("@", "", company),'"}}'))


dispositivos<-outputtw.df$statusSource
dispositivos <-gsub("\"", "", dispositivos)
dispositivos <-gsub("<a href=http[^[:space:]]*", "", dispositivos)
dispositivos <-gsub("<rel=[^[>]]*", "", dispositivos)
dispositivos <- strsplit(dispositivos, ">")
dispositivos <- sapply(dispositivos,function(x) ifelse(length(x) > 1, x[2], x[1]))
dispositivos <-gsub("</a", "", dispositivos)
dispositivos.df<-as.data.frame(dispositivos)
#dispositivos.df$freq<-1
names(dispositivos.df)<-c("class")#,"freq")

summary(dispositivos.df)


head(dispositivos.df)
#gb<-group_by(dispositivos.df, class)
#sm<-summarize(gb,  sum(freq))
#dispositivos.df<-as.data.frame(sm)
dispositivos.df<-as.data.frame(count(dispositivos.df))
names(dispositivos.df)<-c("class","freq")
dispositivos.df<-arrange(dispositivos.df,desc(freq))
dispositivos.df <- dispositivos.df[which(dispositivos.df$freq>=40),]


library(ggplot2)
theme_set(theme_classic())

# Fuente: tabla frecuencia dispositivos
df <-dispositivos.df
colnames(df) <- c("class", "freq")
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title= paste('Dispositivos usuarios',company), 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)
