congress_number_dates <- tibble(congress_number = congress_number,
                                start = c("2001-01-01", "2003-01-01", 
                                          "2005-01-01", "2007-01-01", 
                                          "2009-01-01", "2011-01-01", 
                                          "2013-01-01", "2015-01-01"),
                                end = c("2002-12-31", "2004-12-31",
                                        "2006-12-31", "2008-12-31",
                                        "2010-12-31", "2012-12-31",
                                        "2014-12-31", "2016-12-31")) %>% 
  mutate(start = as.Date(start), end = as.Date(end))

write_rds(congress_number_dates, "~/Documents/gitrepos/data/congress_number_dates.rds")

combine <- read_rds(paste0(data_file, "combine.rds"))

data_file <- "/Volumes/TOSHIBA EXT/gitrepo_data/"

sentiment_combine <- combine %>% 
  filter(party == "D" | party == "R" | party == "I") %>% 
  mutate(name = paste0(firstname, " ", lastname)) %>% 
  group_by(date, name, party, chamber, state) %>% 
  summarise(text = paste0(word, collapse = " "),
            sentiment = sum(value)) %>% 
  ungroup() %>% 
  left_join(lat_long_data %>% 
              filter(State != "DC"), 
            by = c("state" = "State"))

write_rds(sentiment_combine, "/Volumes/TOSHIBA EXT/gitrepo_data/sentiment_combine.rds")

# sentiment_person <- sentiment_combine %>%
#   group_by(name, name, month) %>%
#   summarise(sentiment = sum(value) / length(speakerid),
#             party = unique(party),
#             chamber = unique(chamber),
#             state = unique(state)) %>%
#   ungroup() %>%
#   left_join(lat_long_data %>%
#               filter(State != "DC"),
#             by = c("state" = "State"))
# 
# write_rds(sentiment_person, "/Volumes/TOSHIBA EXT/gitrepo_data/sentiment_person.rds")

gender_percent <- speech_df %>% 
  select(month, firstname, lastname, party, chamber, gender) %>%
  filter(party == "D" | party == "R") %>% 
  mutate(month = as.character(month),
         congress_number = case_when(month >= "2001-01-01" & month <= "2002-12-31" ~ "107th",
                                     month >= "2003-01-01" & month <= "2004-12-31" ~ "108th",
                                     month >= "2005-01-01" & month <= "2006-12-31" ~ "109th",
                                     month >= "2007-01-01" & month <= "2008-12-31" ~ "110th",
                                     month >= "2009-01-01" & month <= "2010-12-31" ~ "111th",
                                     month >= "2011-01-01" & month <= "2012-12-31" ~ "112th",
                                     month >= "2013-01-01" & month <= "2014-12-31" ~ "113th",
                                     month >= "2015-01-01" & month <= "2016-12-31" ~ "114th"),
         name = paste0(firstname, " ", lastname)) %>% 
  distinct(name, chamber, gender, party, congress_number) %>% 
  group_by(chamber, gender, party, congress_number) %>% 
  count()

write_rds(gender_percent, "~/Documents/gitrepos/data/gender_percent.rds")

sentiment_state <- sentiment_combine %>% 
  group_by(state) %>% 
  summarise(average = mean(sentiment))

write_rds(sentiment_state, "~/Documents/gitrepos/data/sentiment_state.rds")


# use_df <- sentiment_combine %>% 
#   filter(date >= "2009-01-01") %>% 
#   group_by(name) %>% 
#   summarise(text = paste0(text, collapse = ""))
# 
# write_csv(use_df, "use_df.csv")

