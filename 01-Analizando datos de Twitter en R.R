####################################################
#### TALLER: ANALIZANDO DATOS DE TWITTER CON R  ####
########  Autor: Francisco Reyes-Vázquez  ##########
##########   Fecha: 12/03/20-17/03/12   ############
####################################################


#---------------------------------------------------
# PRIMERA PARTE: PREPARACIÓN
#---------------------------------------------------

#Instalar paquetes 
remotes::install_github("ropensci/rtweet")  
install.packages("tidyverse")
install.packages("tidytext")
install.packages("lubridate")
devtools::install_github("gadenbuie/tweetrmd")
devtools::install_github("rstudio/webshot2")
evtools::install_github("hadley/emo")
install.packages("tm")
install.packages("wordcloud")
install.packages("zoo")

#Llamar paquetes
library(rtweet)  
library(tidyverse)
library(tidytext)
library(lubridate)
library(tweetrmd) 
library(webshot2) 
library(emo) 
library(tm)
library(wordcloud) 
library(zoo)
#---------------------------------------------------
# SEGUNDA PARTE: IMPORTAR DATOS DE TWITTER
#---------------------------------------------------

#Buscar tweets
tweets <- search_tweets(q="avión presidencial", 
                        n=10000,
                        include_rts=FALSE,
                        `-filter`="replies",
                        lang="es",
                        retryonratelimit = TRUE)

#Explorar tweets importados
tweets%>%
  sample_n(5) %>%
  select(created_at, screen_name, text, favorite_count, retweet_count)

#Guardar datos de twitter
write_as_csv(tweets,"tweets_avión.csv")

#Abrir los datos guardados
tweets2<-read.csv("../Presentación_IBERO/tweets_avión.csv") #la dirección cambia

#---------------------------------------------------
# TERCERA PARTE: EXPLORAR DATOS
#---------------------------------------------------

#Serie de tiempo
tweets2 %>%
  ts_plot("3 hours")

#Tweets por día 
tweets2 %>%
  mutate(day=day(created_at)) %>%
  group_by(day) %>%
  count()

#Ubicación de los tweets
tweets2 %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort=TRUE) %>% 
  top_n(5)

#Tweet más compartido
tweets2%>% 
  arrange(-retweet_count)%>% 
  slice(1)%>% 
  select(created_at, screen_name, text, retweet_count, status_id)

#Ver una imagen de ese tweet
tweet_screenshot(tweet_url("ChumelTorres", "1234875776048095234"))

#Tweets más gustados
tweets2 %>% 
  arrange(-favorite_count) %>%
  top_n(5, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)

#Cuentas más activas
tweets2 %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(5) %>%
  mutate(screen_name = paste0("@", screen_name))

#Emojis más usados
tweets2 %>%
  mutate(emoji=ji_extract_all(text)) %>%
  unnest(cols=c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(5)

#Dividir en palabras y ver las veces que fueron usadas
tweets2 %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)")) %>% 
  filter(str_detect(text,"[^\x27]"))%>%
  unnest_tokens(word, text, token = "words")%>%
  count(word, sort = TRUE)%>%
  top_n(5)


#Identificar las palabras vacías
stopwords2<-stopwords("spanish")
stopwords2<-data.frame(stopwords2)

stopwords2%>%   #Muestra de 5 
  top_n(10)

#Limpiar las palabras
words <- tweets2 %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)")) %>% 
  filter(str_detect(text,"[^\x27]"))%>%
  unnest_tokens(word, text, token = "words")%>%
  filter(!word %in% stopwords2$stopwords2,
         !word %in% str_remove_all(stopwords2$stopwords2, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

#Hacer nube de palabras
words %>% 
  with(wordcloud(word, n, random.order = FALSE, 
                 max.words = 100, colors = "#F29545"))

#--------------------------------------------------
# EXTRA: ANÁLISIS DE SENTIMIENTO
#--------------------------------------------------

install.packages("SentimentAnalysis")

library(SentimentAnalysis)

#Clasificar la siguiente frase
sentiment <- analyzeSentiment("Yeah, this was a great soccer game of the mexican team!")

#Visualizar la polaridad
convertToBinaryResponse(sentiment)

#Crear más oraciones
documents <- c("Wow, I really like the new light sabers!",
               "That book was excellent.",
               "R is a fantastic language.",
               "The service in this restaurant was miserable.",
               "This is neither positive or negative.",
               "The waiter forget about my a dessert, what a poor service!")

#Clasificar las oraciones
sentiment<-analyzeSentiment(documents)

#Visualizar su puntuación
sentiment[c(2,8,12)]

#Visualizar su polaridad
convertToDirection(sentiment[c(2,8,12)])

#Extraer tweets en inglés 
tweets_trump<- search_tweets(q="Trump", 
                        n=500,
                        include_rts=FALSE,
                        `-filter`="replies",
                        lang="en")

#Guardar los tweets
write_as_csv(tweets_trump,"tweets_trump.csv")

#Abrir los datos guardados
tweets_trump<-read.csv("../Presentación_IBERO/tweets_trump.csv") #la dirección cambia


#Crear una función para limpiar
clean.data<-function(text){
  text=gsub("(RT|VIA)((?:\\W*@\\w+)+)","",text)  #Eliminar re-tweets
  text=gsub("@\\w+","",text)                      #Eliminar @
  text=gsub("[[:punct:]]","",text)                #Eliminar signos de puntuaci?n
  text=gsub("[[:digit:]]","",text)               #Eliminar digitos del 0 al 9
  text=gsub("http\\w+","",text)                   #Eliminar html
  text=gsub("[\t]{2,}","",text)                   #Eliminar tabulaciones
  text=gsub("^\\s+|\\s+$","",text)                #Eliminar espacios adicionales
}

#Limpiar
text<-clean.data(tweets_trump$text)

#Clasificar sentimientos
sentiment_trump<-analyzeSentiment(text)

#Ver cuantos son positivos y cuantos negativos en cada diccionario
table(convertToBinaryResponse(sentiment_trump$SentimentGI))
table(convertToBinaryResponse(sentiment_trump$SentimentLM))
table(convertToBinaryResponse(sentiment_trump$SentimentQDAP))

plot(sentiment_trump$SentimentGI,type = "line")

#Promedio móvil
sentiment_trump$mean<-rollmean(sentiment_trump[,2],10,fill = NA) 

#Crear id
sentiment_trump$id<-seq(1,500)

#Graficar el sentimiento (10-03-20 1:36:43 a 1:37:19)
#El sentimiento de los tweets relacionados con trump en 36 segundos
ggplot(sentiment_trump)+
  geom_line(aes(x=id,y=SentimentGI),color="gray70")+
  geom_line(aes(x=id,y=mean),color="salmon")

  

