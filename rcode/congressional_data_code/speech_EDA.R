library(tidyverse)
library(glue)
library(tidytext)
library(plotly)

options(scipen = 999)

speech_df <- read_rds("~/Documents/gitrepos/data/congressional_speech.rds")
congress_house <- read_table2("~/Documents/gitrepos/data/congress_house.txt")
congress_senate <- read_table2("~/Documents/gitrepos/data/congress_senate.txt")
female_senate <- read_table2("~/Documents/gitrepos/data/female_pct_senate.txt")
female_house <- read_table2("~/Documents/gitrepos/data/female_pct_house.txt")
presidents <- read_table2("~/Documents/gitrepos/data/presidents.txt")
#procedural <- read_delim("~/Documents/gitrepos/data/procedural.txt", "|", escape_double = FALSE, trim_ws = TRUE)
combine <- read_rds("~/Documents/gitrepos/data/combine.rds")

# median speech length
median_speech_length <- median(speech_df$word_count, na.rm = TRUE)

speech_df %>% 
  ggplot(aes(word_count)) +
  geom_histogram(bins = 25, fill = "grey70", color = "grey40") +
  geom_vline(xintercept = median_speech_length, color = "red", lty = "dashed") +
  scale_x_log10("# words") +
  ggtitle(glue("Median Review Length is {median_speech_length} Words")) + 
  theme_bw()

# count by chamber
word_counts_chamber <- speech_df %>% 
  group_by(month, chamber) %>% 
  summarise(sum = sum(word_count))

word_counts_chamber %>% 
  ggplot(aes(x = month, y = sum, color = chamber)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Daily Montly Word Counts in Congress by Chamber", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("red", "blue")) +
  theme_bw() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y")

word_counts_gender <- speech_df %>% 
  group_by(month, gender) %>% 
  summarise(ave_word = mean(word_count)) %>% 
  arrange(gender, month)

word_counts_gender %>% 
  ggplot(aes(month, ave_word, color = gender)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts in Congress by Gender", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("red", "blue")) +
  theme_bw() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y")

# house
word_counts_party_house <- speech_df %>% 
  filter(chamber == "H") %>% 
  group_by(month, party) %>% 
  summarise(ave_word = mean(word_count)) %>% 
  arrange(party, month) %>% 
  filter(party == "D" | party == "R") %>% 
  ungroup() %>% 
  mutate(month = as.Date(paste0(month,"-01")))

congress_house_format <- congress_house %>% 
  mutate(majority = if_else(Democrats > Republicans, "democrat", "republican")) %>% 
  select(Start, End, majority)

# republican majority in the house
republican_house_shade <- congress_house_format %>% 
  filter(majority == "republican") %>% 
  select(-majority) %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End)) %>% 
  as.data.frame() %>% 
  filter(Start >= "2001-01-01" & End <= "2017-12-31")

# democrate majority in the house
democrat_house_shade <- congress_house_format %>% 
  filter(majority == "democrat") %>% 
  select(-majority) %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End)) %>% 
  as.data.frame() %>% 
  filter(Start >= "2001-01-01" & End <= "2017-01-01")

# plotting house average word count by party
word_counts_party_house %>%
  ggplot(aes(month, ave_word, color = party)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in House by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y") +
  geom_rect(data=republican_house_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='red', alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=democrat_house_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='blue', alpha=0.1, inherit.aes = FALSE )

# senate plots
word_counts_party_senate <- speech_df %>% 
  filter(chamber == "S") %>% 
  group_by(month, party) %>% 
  summarise(ave_word = mean(word_count)) %>% 
  arrange(party, month) %>% 
  filter(party == "D" | party == "R") %>% 
  ungroup() %>% 
  mutate(month = as.Date(paste0(month,"-01")))

congress_senate_format <- congress_senate %>% 
  mutate(majority = if_else(Democrats > Republicans, "democrat", "republican")) %>% 
  select(Start, End, majority)

# republican majority in the senate
republican_senate_shade <- congress_senate_format %>% 
  filter(majority == "republican") %>% 
  select(-majority) %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End)) %>% 
  as.data.frame() %>% 
  filter(Start >= "2001-01-01" & End <= "2017-12-31")

# democrate majority in the senate
democrat_senate_shade <- congress_senate_format %>% 
  filter(majority == "democrat") %>% 
  select(-majority) %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End)) %>% 
  as.data.frame() %>% 
  filter(Start >= "2001-01-01" & End <= "2017-01-01")

# plotting senate average word count by party
word_counts_party_senate %>%
  ggplot(aes(month, ave_word, color = party)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in Senate by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y") +
  geom_rect(data=republican_senate_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='red', alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=democrat_senate_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='blue', alpha=0.1, inherit.aes = FALSE )

female_percent <- female_senate %>% mutate(chamber = "S") %>% 
  bind_rows(female_house %>% mutate(chamber = "H"))

female_percent %>% 
  filter(chamber == "S") %>% 
  ggplot(aes(x = Congress, y = Congress_pct)) +
  geom_bar(stat = "identity")
  
# sentiment analysis
# groupby gender
sentiment_gender <- combine %>% 
  group_by(gender, month) %>% 
  summarise(sentiment = sum(value) / length(gender))

sentiment_gender %>% 
  ggplot(aes(x = month, y = sentiment, color = gender)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in Senate by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() 

# groupby chamber
sentiment_chamber <- combine %>% 
  group_by(chamber, month) %>% 
  summarise(sentiment = sum(value) / length(chamber))

sentiment_chamber %>% 
  ggplot(aes(x = month, y = sentiment, color = chamber)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in Senate by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() 

# groupby party
sentiment_party_sentate <- combine %>% 
  filter(party == "D"  | party == "R") %>% 
  filter(chamber == "S") %>% 
  group_by(party, month) %>% 
  summarise(sentiment = sum(value) / length(party))

sentiment_party_sentate %>% 
  ggplot(aes(x = month, y = sentiment, color = party)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in Senate by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y") +
  geom_rect(data=republican_senate_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='red', alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=democrat_senate_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='blue', alpha=0.1, inherit.aes = FALSE )

# house
sentiment_party_house <- combine %>% 
  filter(party == "D"  | party == "R") %>% 
  filter(chamber == "H") %>% 
  group_by(party, month) %>% 
  summarise(sentiment = sum(value) / length(party))

sentiment_party_house %>% 
  ggplot(aes(x = month, y = sentiment, color = party)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in house by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y") +
  geom_rect(data=republican_house_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='red', alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=democrat_house_shade, 
            aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill='blue', alpha=0.1, inherit.aes = FALSE )

# congress
sentiment_party <- combine %>% 
  filter(party == "D"  | party == "R") %>% 
  group_by(party, month) %>% 
  summarise(sentiment = sum(value) / length(party))

sentiment_party %>% 
  ggplot(aes(x = month, y = sentiment, color = party)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(y = "Word Count", x = "", 
       title = "Average Montly Word Counts by Speaker in house by Party", 
       subtitle = "Jan. 2001 to Sept. 2016") +
  scale_color_manual(values = c("blue", "red", "purple", "green", "orange", "yellow")) +
  theme_classic() +
  scale_x_date(breaks = "5 years", date_labels = "%B %Y") 
  # geom_rect(data=presidents %>% filter(President == "Bush"), 
  #           aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
  #           fill='red', alpha=0.1, inherit.aes = FALSE) +
  # geom_rect(data=presidents %>% filter(President == "Obama"), 
  #           aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
  #           fill='blue', alpha=0.1, inherit.aes = FALSE )

# by person
sentiment_person <- combine %>% 
  filter(party == "D" | party == "R") %>% 
  mutate(name = paste0(firstname, " ", lastname)) %>% 
  group_by(speakerid, name, month) %>% 
  summarise(sentiment = sum(value) / length(speakerid),
            party = unique(party),
            chamber = unique(chamber)) %>% 
  ungroup()

most_negative_people <- sentiment_person %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(-15, avg_sentiment) 

most_positive_people <- sentiment_person %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(15, avg_sentiment) 

# most negative person on average  
sentiment_person %>% 
  filter(name %in% most_negative_people$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()

# most positive person on average
sentiment_person %>% 
  filter(name %in% most_positive_people$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()

# by chamber - House
sentiment_house <- combine %>% 
  filter(party == "D" | party == "R") %>% 
  filter(chamber == "H") %>%  
  mutate(name = paste0(firstname, " ", lastname)) %>% 
  group_by(speakerid, name, month) %>% 
  summarise(sentiment = sum(value) / length(speakerid),
            party = unique(party),
            chamber = unique(chamber)) %>% 
  ungroup()

most_negative_house <- sentiment_house %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(-15, avg_sentiment) 

most_positive_house <- sentiment_house %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(15, avg_sentiment) 

# most negative person on average  
sentiment_house %>% 
  filter(name %in% most_negative_house$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()

# most positive person on average
sentiment_house %>% 
  filter(name %in% most_positive_house$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()

# by chamber - Senate
sentiment_senate <- combine %>% 
  filter(party == "D" | party == "R") %>% 
  filter(chamber == "S") %>%  
  mutate(name = paste0(firstname, " ", lastname)) %>% 
  group_by(speakerid, name, month) %>% 
  summarise(sentiment = sum(value) / length(speakerid),
            party = unique(party),
            chamber = unique(chamber)) %>% 
  ungroup()

most_negative_senate <- sentiment_senate %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(-15, avg_sentiment) 

most_positive_senate <- sentiment_senate %>% 
  group_by(name) %>% 
  summarise(avg_sentiment = mean(sentiment)) %>% 
  top_n(15, avg_sentiment) 

# most negative person on average  
sentiment_senate %>% 
  filter(name %in% most_negative_senate$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()

# most positive person on average
sentiment_senate %>% 
  filter(name %in% most_positive_senate$name) %>% 
  ggplot(aes(x = name, y = sentiment, fill = party)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "", y = "average sentiment") +
  theme_classic()
