# sourcing environment
source("rcode/runfile.R")

# pulling in data file names
data_files <- list.files(data_path, full.names = TRUE)

# reading in data
blog_data <- lapply(data_files, read_rds)

# putting together a single data.frame
blog_df <- reshape::merge_all(blog_data)
blog_df <- taRifx::unfactor.data.frame(blog_df)

# filtering out documents where scraping didn't work and applying ideology
blog_df_clean <- blog_df %>% 
  filter(!is.na(date)) %>% 
  mutate(ideology = case_when(blog == "The Atlantic" ~ "liberal",
                              blog == "Breitbart" ~ "conservative",
                              blog == "Factcheck.org" ~ "center",
                              blog == "Mother Jones" ~ "liberal",
                              blog == "National Review" ~ "conservative",
                              blog == "Politico" ~ "center",
                              blog == "American Thinker" ~ "conservative",
                              blog == "Daily Kosr" ~ "liberal",
                              FALSE ~ "none"))

# data cleaning functions
blog_df_clean <- blog_df_clean %>% 
  mutate(clean_text = unlist(lapply(text, cleanTextFunction)),
         clean_text = replace_non_ascii(clean_text, ""),
         clean_text = replace_emoji(clean_text),
         clean_text = replace_time(clean_text, replacement = "<time>"),
         clean_text = replace_symbol(clean_text),
         clean_text = replace_date(clean_text, replacement = "<date>"))

# test/train split -------------------------------------------------------------------------------
trainIndex <- createDataPartition(blog_df_clean$ideology, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- blog_df_clean[ trainIndex,]
test  <- blog_df_clean[-trainIndex,]

# write out data
write_csv(train, "data/train.csv")
write_csv(test, "data/test.csv")
