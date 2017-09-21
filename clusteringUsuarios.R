#"@Cruzcampo",
#"@elcorteingles"
#"@Iberia"
#"@Renfe"
#"@Telefonica"
#"@telepizza_es"
#"@Mercadona"

require(mongolite)


company="@Mercadona"
dbfollowers<-mongo(collection = "followers", db = "twitteranalytics", url = "mongodb://localhost", verbose = FALSE)
out.followers<- dbfollowers$find(query =paste0('{"company":"',company,'"}'), fields = '{"_id" : 0}')


head(out.followers)

names(out.followers)

df.followers <- out.followers[,c(2:5,8,9,11,12,14,15,19)]
df.followers<-data.frame(statusesCount=out.followers$statusesCount,
                followersCount=out.followers$followersCount,
                favoritesCount = out.followers$favoritesCount,
                friendsCount =out.followers$friendsCount,
                protected = out.followers$protected, 
                verified= out.followers$verified ,
                location = out.followers$location, 
                lang = out.followers$lang ,
                listedCount = out.followers$listedCount , 
                followRequestSent = out.followers$followRequestSent ,
                numtweets = out.followers$numtweets)



df.followers$protected<-as.factor(df.followers$protected)
df.followers$verified<-as.factor(df.followers$verified)
df.followers$location<-as.factor(df.followers$location)
df.followers$lang<-as.factor(df.followers$lang)
df.followers$followRequestSent<-as.factor(df.followers$followRequestSent)

df.followers$protected<-as.numeric(df.followers$protected)
df.followers$verified<-as.numeric(df.followers$verified)
df.followers$location<-as.numeric(df.followers$location)
df.followers$lang<-as.numeric(df.followers$lang)
df.followers$followRequestSent<-as.numeric(df.followers$followRequestSent)





head(df.followers)


pc = prcomp(df.followers)
plot(pc) 


names(df.followers)
head(df.followers)


wss <- (nrow(df.followers)-1)*sum(apply(df.followers,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(df.followers,centers=i, nstart=20)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

followersCluster1 <- kmeans(pc$x[,1],3, nstart = 20) 

followersCluster2 <- kmeans(df.followers,3, nstart = 20)



#head(followersCluster)

#plot(df.followers , col=followersCluster1$cluster)

plot(df.followers[1:200000,] , col=followersCluster2$cluster)