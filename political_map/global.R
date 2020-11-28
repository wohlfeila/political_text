library(shiny)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(stringr)
library(keras)
library(shinydashboard)
library(htmltools)
library(sf)
library(rgdal)
library(plotly)
library(rsconnect)

data_file <- "Documents/gitrepos/politicalblogclassification/political_map/congressional_text_app_data/"
object_path <- "/Documents/gitrepos/politicalblogclassification/political_map/congressional_text_app_data/shiny_app_classification_objects/"

speech_df <- read_rds(paste0(data_file, "congressional_speech.rds"))
party_congress <- read_rds(paste0(data_file, "party_congress.rds"))
female_senate <- read_rds(paste0(data_file, "female_pct_senate.rds"))
female_house <- read_rds(paste0(data_file, "female_pct_house.rds"))
sentiment_combine <- read_rds(paste0(data_file, "sentiment_combine.rds"))
lat_long_data <- read_rds(paste0(data_file, "statelatlong.rds"))
congress_number_dates <- read_rds(paste0(data_file, "congress_number_dates.rds"))
gender_percent <- read_rds(paste0(data_file, "gender_percent.rds"))
states <- readOGR(paste0(data_file,"cb_2013_us_state_20m/cb_2013_us_state_20m.shp"),
                  layer = "cb_2013_us_state_20m", GDAL1_integer64_policy = TRUE)
embedding_1 <- read_csv(paste0(data_file, "universal_sentence_encoder_embeddings/embedding_1.csv"), 
                        col_names = FALSE)

states_abb <- datasets::state.abb
states_full <- c("All", datasets::state.name)
chamber_names <- c("House" = "H", "Senate" = "S")
congress_number <- unique(gender_percent$congress_number)

# calculated from combine data.frame
average_sentiment <- mean(sentiment_combine$sentiment)

sentiment_state <- sentiment_combine %>% 
  group_by(state) %>% 
  summarise(average = mean(sentiment))

tmp <- tibble(state = states@data[["STUSPS"]]) %>% 
  inner_join(sentiment_state, by = c("state"))

states@data[["ALAND"]] <- tmp$average

pal <- colorQuantile(palette = "PRGn", n = 5, states@data[["ALAND"]])

select_names <- unique(sentiment_combine$name)

use_names <- sentiment_combine %>% 
  filter(date >= "2009-01-01") %>% 
  distinct(name) %>% 
  arrange(name)

use_embed <- cbind(use_names, embedding_1)  

# model ----
new_model <- load_model_hdf5(paste0(object_path,"glove_base_model"))
text_tokenizer <- load_text_tokenizer("congressional_text_app_data/shiny_app_classification_objects/congressional_tokenizer")

