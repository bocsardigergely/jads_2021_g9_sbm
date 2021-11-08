# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')

library(dplyr) 

games_data <- read.csv('./data/clean_games_data.csv')


# Function to establish own stopwords that appear too many times
# (based on frequency)

get_word_freq <- function(dataframe, n = 25) {
  word_list <- dataframe %>% 
    tidytext::unnest_tokens(word, clean_text)
  
  freq_list <- word_list %>% 
    dplyr::count(word, sort = TRUE)
  
  return(c(freq_list[1:n,1]))
}



stm_data <- games_data[c('clean_text', 'location', 'Category', 'launch_date')]


processed <- stm::textProcessor(stm_data$clean_text, metadata = stm_data, lowercase = FALSE, removepunctuation = FALSE, stem=FALSE, sparselevel = 0.997, customstopwords = get_word_freq(stm_data))

stm::plotRemoved(processed$documents, lower.thresh = seq(1, 350, by = 50))



out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 100, upper.thresh = 2800)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

games_test_fit <- stm::stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence =~ location + Category + launch_date, max.em.its = 75, data = out$meta, init.type = "Spectral")
