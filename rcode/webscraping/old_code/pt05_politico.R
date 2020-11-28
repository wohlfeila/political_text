# selecting page
page = seq(1, 10, 1)

politico_df <- c()

for (p in 1:length(page)){
  
  # creating url
  url <- paste0("https://www.politico.com/story/", p)   
  
  # reading url
  webpage <- read_html(url)
  
  # pulling down links
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # removing any node with magazine
  links <- links[!grepl("magazine", links)]
  
  # creating empty vector
  page_df <- c()
  
  for (i in 1:length(links)){
    
    # reading link
    politico_html <- read_html(links[[i]])
    
    # reading text nodes
    politico_text <- politico_html %>% 
      html_nodes("div p")
    
    # pulling down titles from link url
    politico_title <- politico_text[grepl("story-meta__credit", politico_text)]
    politico_title <- politico_title %>% 
      html_text() 
    
    # selecting everything bewteen \n abd |
    title <- trimws(gsub(".*\\n (.+) \\|.*", "\\1", politico_title))
    
    # if there is no title, then add blank so the code doesn't error out
    if (vector_is_empty(title)) { #vector_is_empty is self created function
      title <- "blank"
    }
    
    # pulling author from title after |
    author <- trimws(gsub(".*\\|", "", politico_title))
    author <- gsub("\\/.*", "", author)
    
    # if author is na, then add blank so it doesn't error out
    if (vector_is_empty(author)) {
      author <- "blank"
    }
    
    # extracting blog date
    politico_date <- politico_text[grepl("story-meta__timestamp", politico_text)]
    politico_date <- politico_date %>% 
      html_text()
    
    # formtting date
    date <- as.Date(politico_date, '%m/%d/%Y')
    
    # extracting blog text
    politico_text <- politico_text[grepl("story-text__paragraph", politico_text)]
    politico_text <- politico_text %>% 
      html_text()
    
    # collapsing text
    politico_text <- paste0(politico_text, collapse = " ")
    
    # creating text from scraped data
    tmp_df <- data.frame(date = date, blog = "Politico", author = author, title = title, text = politico_text)
    
    # appending new row to data.frame
    page_df <- rbind(page_df, tmp_df)
  }
  
  politico_df <- rbind(politico_df, page_df)
}

# unfactoring data.frame
politico_df <- taRifx::unfactor.data.frame(politico_df)

# writing out data
write_rds(politico_df, paste0(data_path,"politico_df.rds"))
    