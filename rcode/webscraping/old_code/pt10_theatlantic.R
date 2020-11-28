pages <- seq(1, 7, 1)

atlantic_df <- c()

for (i in pages){
  # creating url
  url <- paste0("https://www.theatlantic.com/politics/?page=",i)
  
  # reading url
  webpage <- read_html(url)
  
  # pulling down links
  links <- webpage %>% 
    html_nodes("div a") %>% 
    html_attr("href")
  
  links <- links[grepl("/politics/archive/", links)]
  links <- paste0("https://www.theatlantic.com", links)
  
  page_df <- c()
  
  for (j in 1:length(links)){
    atlantic_html <- read_html(links[[j]])
    
    # reading text nodes
    atlantic_text <- atlantic_html %>% 
      html_nodes("section p") %>% 
      html_text()
    
    atlantic_text <- paste0(atlantic_text, collapse = " ")
    
    title <- atlantic_html %>% 
      html_nodes(".c-article-header__hed") %>% 
      html_text()
    
    if(length(title) == 0){
      title <- "EMPTY TITLE"
    } else{
      title <- title
    }
    
    author <- atlantic_html %>% 
      html_nodes("span a") %>% 
      html_text()
    
    author <- author[1]
    
    date <- atlantic_html %>% 
      html_nodes("div time") %>% 
      html_text()
    
    tmp_df <- data.frame(date = date, blog = "The Atlantic", author = author, title = title, text = atlantic_text)
    page_df <- rbind(tmp_df, page_df)
  }
  atlantic_df <- rbind(atlantic_df, page_df)
}

atlantic_df <- taRifx::unfactor.data.frame(atlantic_df)

atlantic_df <- unique(atlantic_df)

# pulling current data time for date manipulation
current_date_time <- Sys.time()

# pulling rows without a date, and converting it into date
non_date_rows <- atlantic_df %>% 
  filter(grepl("ET", date)) %>% 
  mutate(date2 = as.numeric(gsub("([0-9]+).*$", "\\1", date)),
         # converting date hours into seconds
         sec_to_hour = date2 * 3600, 
         # converting hours into proper date format
         date = as.Date(current_date_time - sec_to_hour)) %>% 
  select(-sec_to_hour, -date2)

# converting date column to class date
date_rows <- atlantic_df %>% 
  filter(!grepl("ET", date)) %>% 
  mutate(date = as.Date(trimws(date), format = "%B %d, %Y"))

# combining data.frames together
atlantic_df <- rbind(non_date_rows, date_rows) 

# writing out data.frame
write_rds(atlantic_df, paste0(data_path,"atlantic_df.rds"))
