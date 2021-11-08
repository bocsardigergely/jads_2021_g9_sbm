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

print(paste("First heuristic to determine the number of topics:",round(sqrt(length(games_data$clean_text)/2),0)))


timea<-Sys.time()
many_models <- data_frame(K = c(10, 20, 50, 70, 90, 100, 125, 150,200)) %>%
  mutate(topic_model = furrr::future_map(K, ~stm::stm(documents=docs, vocab=vocab, K = ., max.em.its = 10,
                                          verbose = FALSE)))

print(Sys.time()-timea)

save(many_models, file = 'num_topics_df.Rda')




games_test_fit <- stm::stm(documents = out$documents, vocab = out$vocab, K = 42, prevalence =~ location + Category + launch_date, max.em.its = 250, reportevery = 1, data = out$meta, init.type = "Spectral")
