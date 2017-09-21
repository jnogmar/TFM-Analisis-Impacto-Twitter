
install.packages("translate")
library(translate)


require(translate)

emotions = read.csv( "D:/Documentos/Máster Universitario Visual Analytics and Big Data/TFM/Source/emotions.csv",sep=",")
colnames(subjectivities)<-c("word","polarity")

subjectivities = read.csv( "D:/Documentos/Máster Universitario Visual Analytics and Big Data/TFM/Source/subjectivity.csv",sep=",")
colnames(subjectivities)<-c("word","typesubj","polarity")


set.key("---------------------------------------")
traducir <-function(x){
  return(translate(x,source = "en",target = "es",key = get.key())[[1]])
}

emotions$word<-sapply(emotions$word,traducir)
subjectivities$word<-sapply(subjectivities$word,f)

write.csv(emotions,file = "D:/Documentos/Máster Universitario Visual Analytics and Big Data/TFM/Source/emotions2.csv",sep=",",col.names = FALSE,quote = FALSE,row.names = FALSE)
write.csv(subjectivities,file = "D:/Documentos/Máster Universitario Visual Analytics and Big Data/TFM/Source/subjectivity2.csv",sep=",",col.names = FALSE,quote = FALSE,row.names = FALSE)

