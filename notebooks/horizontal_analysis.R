# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')

#loading the data in formats we like
#####
games_src <- read.csv('./data/final_games.csv', row.names = 1, header= TRUE)
tech_src <- read.csv('./data/final_tech.csv', row.names = 1, header= TRUE)
design_src <- read.csv('./data/final_design.csv', row.names = 1, header= TRUE)


setup_data <- function(dataframe){
  
  dataframe$location <- as.factor(dataframe$location)
  dataframe$Category <- as.factor(dataframe$Category)
  dataframe$degree_of_diff <- as.numeric(gsub("\\[|\\]", "", dataframe$degree_of_diff))
  dataframe$top_country <- as.factor(dataframe$top_country)
  dataframe$Staff_recommended <- as.logical(dataframe$Staff_recommended)
  dataframe$pledged_binary <- as.logical(dataframe$pledged_binary, )
  dataframe$pledged_percentage <- NULL
  
  degree <- dataframe[, c(1:9)]
  
  return(list('degree'= degree, 'full' = dataframe))
}
#####


#DESIGN DATASET ANALYSIS
##### 
design <- setup_data(design_src)
vec_model <- glm(pledged_binary ~ ., data = design$degree, family = binomial(link = 'logit'))
summary(vec_model)

adjusted_vec_model <- lm(pledged_binary ~ X8 + X11 + X14 + X16 + X44 + X48, data = design$vector)
summary(adjusted_vec_model)

#####

#GAMES DATASET ANALYSIS
#####
games <- setup_data(games_src)
vec_model <- glm(pledged_binary ~ . - pledged_percentage, data = games$vector, family = binomial(link = logit))
summary(vec_model)

adjusted_vec_model <- glm(pledged_binary ~ X3 + X6 + X8 +X14 + X15 + X21 + X27 + X28 + X29 + X37 + X41 + X45 + X46, data = games$vector, family = binomial(link = logit))
summary(adjusted_vec_model)
#####


#TECH DATASET ANALYSIS
#####
tech <- setup_data(tech_src)
vec_model <- lm(pledged_binary ~ . - pledged_percentage, data = tech$vector)
summary(vec_model)

#####


