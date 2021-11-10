# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')

library(stm)
library(dplyr)
library(tidytext)
library(bigrquery)
library(purrr)
library(tidyr)
library(ggplot2)
library(furrr)
library(stminsights)
library(quanteda)

source <- read.csv('./data/clean_design_data.csv')


# Function to establish own stopwords that appear too many times
# (based on frequency)

get_word_freq <- function(dataframe, n = 25) {
  word_list <- dataframe %>% 
    tidytext::unnest_tokens(word, clean_text)
  
  freq_list <- word_list %>% 
    dplyr::count(word, sort = TRUE)
  
  return(c(freq_list[1:n,1]))
}

stm_data <- source[c('clean_text', 'location', 'Category', 'launch_date')]


processed <- textProcessor(stm_data$clean_text, metadata = stm_data, lowercase = FALSE, removepunctuation = FALSE, stem=FALSE, sparselevel = 0.997, customstopwords = get_word_freq(stm_data))

plotRemoved(processed$documents, lower.thresh = seq(1, 350, by = 50))


out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 100, upper.thresh = 2800)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

print(paste("First heuristic to determine the number of topics:",round(sqrt(length(source$clean_text)/2),0)))

many_models <- data_frame(K = c(20, 50, 100, 150)) %>%
  mutate(topic_model = furrr::future_map(K, ~stm::stm(documents=docs, vocab=vocab, K = ., max.em.its = 10,
                                          verbose = FALSE)))

heldout <- make.heldout(documents=docs, vocab=vocab)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, documents=docs),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, documents=docs),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be a number around 50")


k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(20, 50, 100, 150)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


stm_fit <- stm(documents = out$documents, vocab = out$vocab, K = 50, prevalence =~ location + Category + launch_date, max.em.its = 20, reportevery = 5, data = out$meta, init.type = "Spectral", gamma.prior='L1')

plot(stm_fit, n=5,labeltype = "frex", topics = 25:50, type="summary")

labelTopics(stm_fit,n=10) 

barplot(stm_fit$theta[1,],names.arg = paste("t",1:50))


summary(cbind(meta,stm_fit$theta))

write.csv(cbind(meta,stm_fit$theta), './data/tech_stm_theta_with_meta.csv')
