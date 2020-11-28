month <- seq(1, 12, 1)

national_review_df <- c()

for (j in 1:length(month)){
  url <- paste0("https://www.nationalreview.com/2019/", j, "/")
  
  webpage <- read_html(url)
  
  links <- webpage %>% 
    html_nodes("h4 a") %>% 
    html_attr("href")
  
  month_df <- c()
  
  for (i in 1:length(links)){
    
    national_html <- read_html(links[[i]])
    
    national_text <- national_html %>% 
      html_nodes("p") 
    
    national_text <- national_text[!grepl("p\\ class|National\\ Review", national_text)]
    
    national_text <- national_text %>% 
      html_text()
    
    national_text <- paste0(national_text, collapse = " ")
    
    national_title <- national_html %>% 
      html_node("div h1") %>% 
      html_text()
    
    author <- national_html %>% 
      html_node(".author") %>% 
      html_text()
    
    date <- national_html %>% 
      html_node("div time") %>% 
      html_text()
    
    # creating text from scraped data
    tmp_df <- data.frame(date = date, blog = "National Review", author = author, title = national_title, text = national_text)
    
    # appending new row to data.frame
    month_df <- rbind(month_df, tmp_df)
    
  }
  national_review_df <- rbind(national_review_df, month_df)
}

# re-formatting dates and removing duplicate rows
national_review_df <- taRifx::unfactor.data.frame(national_review_df) %>% 
  dplyr::distinct(date, blog, author, title, text) %>% 
  mutate(date = as.Date(date, '%B %d, %Y'))

# writing out data.frame
write_rds(national_review_df, paste0(data_path,"national_review_df_2.rds"))


    