library(tidyverse)

file_list <- list.files("~/Documents/gitrepos/data/hein-daily/", full.names = TRUE)

speaker_files <- file_list[grepl("SpeakerMap", file_list)]
speech_files <- file_list[grepl("speeches", file_list)]
descr_files <- file_list[grepl("descr", file_list)]

speaker_combine <- lapply(speaker_files, function(x){
  read_delim(x, "|", escape_double = FALSE,
             col_types = cols(speech_id = col_character()),
             trim_ws = TRUE)
  })

speech_combine <- lapply(speech_files, function(x){
  read_delim(x, "|", escape_double = FALSE,
             col_types = cols(speech_id = col_character()),
             trim_ws = TRUE)
  })

descr_combine <- lapply(descr_files, function(x){
  read_delim(x, "|", escape_double = FALSE,
             col_types = cols(speech_id = col_character()),
             trim_ws = TRUE)
})

speaker_df <- do.call(rbind.data.frame, speaker_combine)
speech_df <- do.call(rbind.data.frame, speech_combine)
descr_df <- do.call(rbind.data.frame, descr_combine)

combined_df <- speech_df %>% 
  left_join(descr_df %>% 
              select(speech_id, date, char_count, word_count), 
            by = c("speech_id")) %>% 
  left_join(speaker_df %>% 
              select(-district, -nonvoting), 
            by = c("speech_id")) %>% 
  filter(!is.na(party)) %>% 
  mutate(date = as.Date(as.character(date), "%Y%m%d"))

# re-formatting documents to condense same sequential speakers as one text
speech_clean <- combined_df %>% 
  group_by(date) %>% 
  mutate(prev_id = lag(speakerid),
         same_speaker = if_else(speakerid == prev_id, 0, 1),
         same_speaker = replace_na(same_speaker, 1),
         grp = cumsum(same_speaker)) %>% 
  ungroup() %>% 
  group_by(date, grp) %>% 
  summarise(text = paste0(speech, collapse = " "),
            speakerid = unique(speakerid),
            lastname = unique(lastname), 
            firstname = unique(firstname),
            chamber = unique(chamber), 
            state = unique(state), 
            gender = unique(gender), 
            party = unique(party)) %>% 
  mutate(word_count = str_count(text, "\\w+"),
         month = format(as.Date(date), "%Y-%m"),
         month = as.Date(paste0(month, "-01"))) %>% 
  select(-grp) %>% 
  ungroup()

write_rds(speech_clean, "~/Documents/gitrepos/data/congressional_speech.rds")
