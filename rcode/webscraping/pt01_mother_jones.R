# pages we want to pull data for
page <- seq(1, 50, 1)

# creating empty vector
motherjones_df <- c()

for (j in page){
  
  # specifying the url for desired website to be scraped
  url <- paste0("https://www.motherjones.com/politics/page/",j,"/")
  
  # reading the HTML code from the website
  webpage <- read_html(url)
  
  # pulling links from url
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # creating empty vecotr
  month_df <- c()
  
  for (i in 1:length(links)){
    
    # reading in first link
    jones_html <- read_html(links[[i]])
    
    # pulling text nodes from link
    jones_text <- jones_html %>% 
      html_nodes("article p")
    
    # removing nodes with classliframe nodes
    jones_text <- jones_text[!grepl("p\\ class|iframe", jones_text)]
    
    # reading in text
    jones_text <- jones_text %>% 
      html_text()
    
    jones_text <- sub("(pic.twitter[^ ]*)","",jones_text)
    jones_text <- sub("(http[^ ]*)","",jones_text)
    
    # collapsing text into on string
    jones_text <- paste0(jones_text, collapse = " ")
    
    # reading title nodes
    jones_title <- jones_html %>% 
      html_nodes("header h1") %>% 
      html_text()
    
    # reading author nodes
    author <- jones_html %>% 
      html_nodes(".url") %>% 
      html_text()
    
    # collapsing text
    author <- paste(author, collapse = " ")
    
    # reading article tags
    tags <- jones_html %>% 
      html_nodes("li a") %>% 
      html_attr("rel")
    
    tags <- tibble(tags = unlist(tags))
    
    tags2 <- jones_html %>% 
      html_nodes("li a") %>% 
      html_text()
    
    tags2 <- tibble(tags2 = unlist(tags2))
    
    tmp <- cbind(tags, tags2) 
    
    tmp <- tmp %>% filter(tags == "tag") %>% pull(tags2)
    
    jones_tags <- paste0(tmp, collapse = ";")
    
    # reading date nodes
    date <- jones_html %>% 
      html_nodes(".dateline") %>% 
      html_text()
    
    # building data.frame from scraped data
    tmp_df <- data.frame(date = date, blog = "Mother Jones", author = author, 
                         title = jones_title, text = jones_text, tags = jones_tags)
    
    # appending data to existing data.frame
    month_df <- rbind(month_df, tmp_df)
  }
  
  motherjones_df <- rbind(motherjones_df, month_df)
}

# removing factors from data.frame
motherjones_df <- taRifx::unfactor.data.frame(motherjones_df)

# pulling current data time for date manipulation
current_date_time <- Sys.time()

# pulling rows without a date, and converting it into date
non_date_rows <- motherjones_df %>% 
  filter(grepl("hours", date)) %>% 
  mutate(date2 = as.numeric(gsub("([0-9]+).*$", "\\1", date)),
         # converting date hours into seconds
         sec_to_hour = date2 * 3600, 
         # converting hours into proper date format
         date = as.Date(current_date_time - sec_to_hour)) %>% 
  select(-sec_to_hour, -date2)

# converting date column to class date
date_rows <- motherjones_df %>% 
  filter(!grepl("hours", date)) %>% 
  mutate(date = as.Date(date, '%B %d, %Y')) 

# combining data.frames together
motherjones_df <- rbind(non_date_rows, date_rows) %>% 
  dplyr::distinct(title, .keep_all = TRUE) %>% 
  filter(text != "")

# writing out data.frame
write_csv(motherjones_df, paste0(blog_data_path,"motherjones_df.csv"))
