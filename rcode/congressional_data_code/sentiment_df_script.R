library(tidyverse)
library(glue)
library(tidytext)

# speech_df <- read_rds("~/Documents/gitrepos/data/congressional_speech.rds")
#
# afinn <- get_sentiments("afinn")
#
# sentiment_one <- speech_df %>%
#   filter(month <= "2005-01-01") %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words, by = "word") %>%
#   inner_join(afinn, by = "word")
#
# sentiment_two <- speech_df %>%
#   filter(month >= "2005-02-01" & month <= "2010-01-01") %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words, by = "word") %>%
#   inner_join(afinn, by = "word")
#
# sentiment_three <- speech_df %>%
#   filter(month >= "2010-02-01") %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words, by = "word") %>%
#   inner_join(afinn, by = "word")
#
# combine <- rbind(sentiment_one, sentiment_two, sentiment_three)
#
# write_rds(combine, "~/Documents/gitrepos/data/combine.rds")


