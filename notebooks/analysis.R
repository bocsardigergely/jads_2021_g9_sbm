# setting the working directory to my local computer
setwd('C:/Gergo-mappa/projects/programming/projects/jads/sbm/jads_2021_g9_sbm')

#loading the data in formats we like
games_src <- read.csv('./data/final_games.csv', row.names = 1, header= TRUE)
tech_src <- read.csv('./data/final_tech.csv', row.names = 1, header= TRUE)
design_src <- read.csv('./data/final_design.csv', row.names = 1, header= TRUE)


setup_data <- function(dataframe){
  
  dataframe$location <- as.factor(dataframe$location)
  dataframe$Category <- as.factor(dataframe$Category)
  dataframe$degree_of_diff <- as.numeric(gsub("\\[|\\]", "", dataframe$degree_of_diff))
  dataframe$top_country <- as.factor(dataframe$top_country)
  dataframe$Staff_recommended <- as.logical(dataframe$Staff_recommended)
  dataframe$pledged_binary <- as.logical(dataframe$pledged_binary)
  
  meta <- dataframe[, c(1:10)]
  vec <- cbind(dataframe[, c(11:50)], pledged_binary = dataframe$pledged_binary, peldged_percentage =dataframe$pledged_percentage)
  
  return(list('meta'=meta, 'vector' =vec))
}

design <- setup_data(design_src)

lm <- lm(pledged_binary ~ degree_of_diff, data = design$meta)
summary(lm)
