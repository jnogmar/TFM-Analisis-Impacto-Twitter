require(twitteR)
require(httpuv)
#Claves obtenidas al dar de alta la aplicación en nuestra cuenta de desarrolladores de Twitter
consumerkey ="---------consumerkey" 
consumerSecret = "-----------------------consumerSecret"
#LLamada a la función para la configuración de acceso al API de Twitter  
setup_twitter_oauth(consumer_key=consumerkey,consumer_secret=consumerSecret)
