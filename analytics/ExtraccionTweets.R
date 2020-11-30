# Installation of required packages
install.packages("tm")
install.packages("rtweet")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("knitr")
install.packages("tidytext")
install.packages("syuzhet")


# Load package
library(rtweet)
library(ggplot2)
library(tidyverse)
library(knitr)
library(tidytext)
library(syuzhet)



# Data access key (Twitter)

api_key             <- "lhoFGi7WwTCNweSwjLAf0pcsO"
api_secret          <- "HX5YP7MaRb0pri3G73tbIIqvv8qbUGiptsY4fsL9bCN6YjN9zv"
access_token        <- "920582805981188097-kydfkJ915usop34k1VQd9uMBVKTxnaL"
access_token_secret <- "73LOfzcjfIPlUUNoDVOVXYt5uEpBPaiM16EnSk9HD7pUO"
twitter_app         <- " Twitter Data"


# Data token (Twitter)
twitter_token <- create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)


# Extraction function 
extraccion_tweets <- function(usuario, maxtweets = 100, output_file_name = NULL){
  # This function extracts the tweets posted by a user and stores them in an rds file.
  # If a file with the same name exists: reads, concatenates the new tweets and overwrites it if necessary.
  
  # Arguments:
  # usuario: twitter user ID
  # maxtweets: number of tweets recovered
  # output_file_name: name of the write file

  # If the name of the archive file is not specified, a default name is created.
  if(is.null(output_file_name)){
    output_file_name <- paste0("datos_tweets_", usuario, ".rds")
  }
  
  # If the storage file does not exist, a new one is created with the results of the first recovery
  if(!(output_file_name %in% list.files())){
    datos_new <- get_timeline(user = usuario, n = maxtweets, 
                              parse = TRUE, check = TRUE,
                              include_rts = FALSE)
    saveRDS(object = datos_new, file = output_file_name)
    print(paste0("Nuevo fichero creado: ", output_file_name))
  }else{
    
    # Old data validation
    datos_old <- readRDS(file = output_file_name)
    # Identification of the last ID recovered
    ultimo_id <- tail(datos_old, 1)["status_id"] %>% pull()  %>% as.numeric()
    # To avoid retrieving the last tweet from the previous query, the Id is increased by 1
    ultimo_id = ultimo_id + 1
    # To avoid compatibility errors, all number columns are converted to character
    datos_old <- map_if(.x = datos_old, .p = is.numeric, .f = as.character)
    # Extract new tweets
    datos_new <- get_timeline(user = usuario, n = maxtweets,
                              max_id = ultimo_id, parse = TRUE, 
                              check = TRUE, include_rts = FALSE)
    datos_new <- map_if(.x = datos_new, .p = is.numeric, .f = as.character)
    # Concatenation of old and new data
    datos_concatenados <- bind_rows(datos_old, datos_new)
    saveRDS(object = datos_concatenados, file = output_file_name)
    print(paste("Número total de tweets:", nrow(datos_concatenados)))
  }
}


# User or Users to extract

usuarios = c("ecuarauz")
n <- 1 # Number of iterations, adjust as required
mxt <- 130
for (k in 1:n) {
  print(paste0("ciclo: ", k))
  # Extract maximum 3200 tweets in ongeveer 15 minuten
  for (j in 1:length(usuarios)) {
    extraccion_tweets(usuario  = usuarios[j], maxtweets  = mxt)
  }
  # Creates a pause until the tweeting permission is restarted
  if(k<n){
    (60*rate_limit(query = "get_timeline")$reset[[1]]) %>% 
      round() %>% 
      Sys.sleep()
  }
}

tweets <- rbind(readRDS("datos_tweets_@ecuarauz.rds"))

tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 



# Features selection 
tweets <- tweets %>% select(screen_name, created_at, status_id, is_retweet, favorite_count, retweet_count, text)

# Features rename
tweets <- tweets %>% 
  rename(autor = screen_name, fecha = created_at, texto = text,
         tweet_id = status_id, is_retweet = is_retweet,
         cont_fav = favorite_count, cont_rt = retweet_count)
head(tweets)



cleanTweet <- function(text, users = TRUE, hashtags = TRUE, token = TRUE, stopword = TRUE){
  # Features
  # text: Text to clean.  It does accept character formatting.
  # users: Leave or delete twitter users (delete the @ anyway).
  # hashtags: Leave or delete twitter hashtags (deletes the # anyway).
  # token: token the result.
  # stopword: delete the stop words
  
  # Removing the links in the tweets
  tweets <- gsub("http.*","",text)
  tweets <- gsub("https.*","",tweets)
  
  # Removing the hashtags and users in the tweets
  if(!hashtags){tweets <- gsub("#\\w+","",tweets)}
  if(!users){tweets <- gsub("@\\w+","",tweets)}
  
  # Removing punctuation marks, numbers and text with numbers
  tweets <- gsub("[[:punct:]]","",tweets)
  tweets <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets)
  tweets <- stringr::str_replace_all(tweets, "\\p{quotation mark}", "")
  tweets <- gsub("\\n", " ",tweets)
  tweets <- stringr::str_replace_all(tweets,"[\\s]+", " ")
  tweets <- stringr::str_replace_all(tweets," $", "")
  #tweets <- chartr('??????','aeioun',tweets) # Problemas con tildes
  
  # Remove Emojis
  tweets <- iconv(tweets, from = "UTF-8", to = "latin1", sub = "byte")
  tweets <- gsub("<\\w+>","",tweets)
  
  # Transforms everything into a minuscule
  tweets <- tolower(tweets)
  if(stopword){
    tweets <- tm::removeWords(tweets, 
                              iconv(tm::stopwords("spanish"), 
                                    from = "", 
                                    to = "UTF-8",
                                    sub = "byte"))
  }
  if(token){
    # Individual word tokenization
    tweets <- stringr::str_split(tweets, " ")[[1]]
    # Elimination of tokens with a length < 2
    tweets <- purrr::keep(.x = tweets,
                          .p = function(x){stringr::str_length(x) > 1})
  }
  return(tweets)
}




# Tweet cleaning
cleanTweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto, .f = cleanTweet))

# Sorting clean data
tweets_tidy <- cleanTweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy)



####################################################################################################

# gráfico número de tweets por autor
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%d-%m", date_breaks = "1 day") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


# gráfico número de tweets
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m-%d"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

tweets_tidy %>%  ggplot(aes(x = autor, fill = autor)) + geom_bar() +
  coord_flip() + 
  theme_bw() + labs(title = "Número de palabras por usuario")

tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor, fill = autor)) + geom_bar() + coord_flip() + 
  theme_bw() + labs(title = "Número de palabras distintas por usuario")


tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%   
  group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud, fill = autor)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw() + 
  labs(title = "Longitud media de los tweets por usuario")


#palabras mas usadas
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=10)

#grafico de las diez mas usadas
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 2, drop = TRUE)





SentTweet <- function(text, onlyResult = FALSE, summary = TRUE){
  # Variables:
  # text: texto a revisar
  # onlyResult: si TRUE, devuelve solo el conteo.
  
  #Transformamos la base de textos importados en un vector para
  #poder utilizar la función get_nrc_sentiment
  
  if(summary){text <- as.vector(text)}
  #Aplicamos la función indicando el vector y el idioma y creamos
  #un nuevo data frame llamado emocion.df
  emocion.df <- get_nrc_sentiment(char_v = text, language = "spanish")
  #Traduce nombre de emociones a español
  colnames(emocion.df)[1] <- "ira"
  colnames(emocion.df)[2] <- "anticipacion"
  colnames(emocion.df)[3] <- "aversion"
  colnames(emocion.df)[4] <- "miedo"
  colnames(emocion.df)[5] <- "alegria"
  colnames(emocion.df)[6] <- "tristeza"
  colnames(emocion.df)[7] <- "sorpresa"
  colnames(emocion.df)[8] <- "confianza"
  colnames(emocion.df)[9] <- "negativo"
  colnames(emocion.df)[10] <- "positivo"
  if(summary){
    #Unimos emocion.df con el vector tweets.df para ver como
    #trabajó la función get_nrc_sentiment cada uno de los tweets
    #   emocion.df2 <- cbind(tweets.df2, emocion.df)
    #Creamos un data frame en el cual las filas serán las emociones
    #y las columnas los puntajes totales
    #Empezamos transponiendo emocion.df
    
    emocion.df3 <- data.frame(t(emocion.df))
    #Sumamos los puntajes de cada uno de los tweets para cada emoción
    
    emocion.df3 <- data.frame(rowSums(emocion.df3))
    #Nombramos la columna de puntajes como cuenta
    names(emocion.df3)[1] <- "cuenta"
    #Dado que las emociones son los nombres de las filas y no una variable
    #transformamos el data frame para incluirlas dentro
    
    emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)
    #Quitamos el nombre de las filas
    rownames(emocion.df3) <- NULL
    #Devuelve el data frame resultante
    if(onlyResult==TRUE){emocion.df4 <- emocion.df3[,2]}
    if(onlyResult==FALSE){emocion.df4 <- emocion.df3}
  }
  if(!summary){
    emocion.df <- data.frame(texto = text, emocion.df, stringsAsFactors = FALSE)
    #Devuelve el data frame resultante
    if(onlyResult==TRUE){emocion.df4 <- emocion.df[,-1]}
    if(onlyResult==FALSE){emocion.df4 <- emocion.df}
  }
  emocion.df4
}
calcSent <- function(x){
  x2 <- x[,8:15]
  y <- x
  # x2 <- x2[rowSums(x2)>0,]
  l=1
  for (k in 1:dim(x)[1]) {
    a <- which.max(x2[k,])
    mat <- x2[k, a]==x2[k,]
    mat <- names(x2[k,])[mat]
    if(sum(x2[k,])==0){
      y[l,1:dim(x)[2]] <- x[k,]
      y[l,dim(x)[2]+1] <- ""
      l <- l + 1
    }
    if(sum(x2[k,])>0){
      for (i in 1:length(mat)) {
        y[l,1:dim(x)[2]] <- x[k,]
        y[l,dim(x)[2]+1] <- mat[i]
        l <- l + 1
      }
    }
  }
  names(y)[dim(y)[2]] <- "emocion"
  y
}



sent <- SentTweet(tweets_tidy$token, onlyResult = FALSE, summary = FALSE)

tweets_sent <- cbind(tweets_tidy, sent[,-1], tipo =
                       ifelse(sent$positivo>sent$negativo,"positivo",
                              ifelse(sent$positivo<sent$negativo,
                                     "negativo","neutro")))
tweets_sent2 <- calcSent(tweets_sent)

tweets_sent$fecha <- as.Date(tweets_sent$fecha)
tweets_sent2$fecha <- as.Date(tweets_sent2$fecha)

# positivo - negativo
# tweets_sent[tweets_sent$tipo!="",] %>%
tweets_sent[sent$positivo!=0|sent$negativo!=0,] %>%
  count(autor, tipo) %>%
  group_by(autor) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(autor, Proporcion, fill = tipo) +
  geom_col() +
  theme(legend.position = "top")


# emociones
tweets_sent2[tweets_sent2$emocion!="",] %>%
  count(autor, emocion) %>%
  group_by(autor) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(autor, Proporcion, fill = emocion) +
  geom_col() +
  theme(legend.position = "top")

tweets_sent2[tweets_sent2$emocion!="",] %>%
  group_by(autor, fecha) %>%
  count(emocion) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(fecha, Proporcion, fill = emocion) +
  geom_col(width = 1) +
  facet_grid(autor~.) +
  scale_x_date(expand = c(0, 0)) +
  theme(legend.position = "top")



