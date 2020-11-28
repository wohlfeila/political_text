library(keras)
library(tidyverse)
library(glue)
library(progress)
library(tidytext)

# create file paths
data_file <- "/Volumes/TOSHIBA EXT/gitrepo_data/"

# read in speech data
speech_df <- read_rds(paste0(data_file, "congressional_speech.rds"))

# aggregate speech data up to daily by speaker, convert target variable to numeric and remove stop words
tmp <- speech_df %>%
  mutate(name = paste0(firstname, " ", lastname)) %>%
  group_by(name, date, party) %>%
  summarise(text = paste0(text, collapse = " ")) %>%
  ungroup() %>%
  mutate(party = if_else(party == "D", 1, 0),
         text = iconv(text, "ASCII", "UTF-8", sub="byte"))

# remove stopwords in chunks
tmp1 <- tmp[1:54682, ] %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(date, name, party) %>% 
  summarise(text = paste0(word, collapse = " "))
  
tmp2 <- tmp[54683:109364, ] %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(date, name, party) %>% 
  summarise(text = paste0(word, collapse = " "))

tmp3 <- tmp[109365:164046, ] %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(date, name, party) %>% 
  summarise(text = paste0(word, collapse = " "))

tmp4 <- tmp[164047:nrow(tmp), ] %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  group_by(date, name, party) %>% 
  summarise(text = paste0(word, collapse = " "))

tmp_combine <- rbind(tmp1, tmp2, tmp3, tmp4)

# randomizing data
set.seed(42)
rows <- sample(nrow(tmp_combine))
model_data <- tmp_combine[rows, ]

# converting all text to lower case
clean_text <- tolower(model_data$text)

# seperating out lables
labels <- model_data$party

# train/test split - taking a sample and randomizing data
train_index <- sample.int(n = nrow(model_data), size = floor(0.80 * nrow(model_data)), replace = F)

# creating test/train data sets
train_text <- clean_text[train_index]
test_text <- clean_text[-train_index]
train_labels <- labels[train_index]
test_labels <- labels[-train_index]

congressional_text_data <- list(train_text, train_labels, test_text, test_labels)

write_rds(congressional_text_data, paste0(data_file, "congressional_text_data_stop_words_removed.rds"))