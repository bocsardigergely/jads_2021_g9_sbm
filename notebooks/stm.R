# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')


games_data <- read.csv('./data/clean_games_data.csv')

dummy <- head(games_data, n=5)

# Function to establish own stopwords that appear too many times
# (based on frequency)

get_word_freq <- function(dataframe, n = 25) {
  word_list <- dataframe %>% 
    tidytext::unnest_tokens(word, clean_text)
  
  freq_list <- word_list %>% 
    count(word, sort = TRUE)
  
  return(c(freq_list[1:n,1]))
}

summary(dummy)

stm_data <- dummy[c('clean_text', 'location', 'Category', )]



