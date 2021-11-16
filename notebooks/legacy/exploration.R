# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')


# installing necessary packages
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('wordcloud2')
install.packages('tm')


#loading the data
games_data <- read.csv('./data/clean_games_data.csv')             
tech_data <- read.csv('./data/clean_tech_data.csv')
design_data <- read.csv('./data/clean_design_data.csv')
                 

# building the wordclouds

draw_wordcloud <- function(dataframe) {
  
  text <- dataframe$clean_text
  docs <- tm::Corpus(tm::VectorSource(text))
  dtm <- tm::TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  
  set.seed(1234)
  return(wordcloud2::wordcloud2(data=df, size=1.6, color='random-dark', shape = 'square'))
  
}

draw_wordcloud(tech_data)
