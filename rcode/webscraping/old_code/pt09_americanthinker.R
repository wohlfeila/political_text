# creating url
url <- "https://www.americanthinker.com/articles/2019/12/"
  
# reading url
webpage <- read_html(url)
  
# pulling down links
links <- webpage %>% 
  html_nodes("div a") %>% 
  html_attr("href")
  
links <- links[grepl("/articles", links)]
links <- unique(links)
links <- paste0("https://www.americanthinker.com", links)
  
# creating empty vector
americanthinker_df <- c()
  
for (i in 1:length(links)){
  # reading link
  americanthinker_html <- read_html(links[[i]])
  
  # reading text nodes
  americanthinker_text <- americanthinker_html %>% 
    html_nodes("div p") %>% 
    html_text()
  
  americanthinker_text <- americanthinker_text[-length(americanthinker_text)]
  americanthinker_text <- paste0(americanthinker_text, collapse = " ")
  
  author <- americanthinker_html %>% 
    html_nodes("div a") %>% 
    html_text()
  
  author <- author[36]
  
  date <- americanthinker_html %>% 
    html_nodes("div div") %>% 
    html_text()
  
  date <- date[13]
  
  title <- americanthinker_html %>% 
    html_nodes("div h1") %>% 
    html_text()
  
  title <- title[2]
  
  tmp_df <- data.frame(date = date, blog = "American Thinker", author = author, title = title, text = americanthinker_text)
  
  # appending new row to data.frame
  americanthinker_df <- rbind(americanthinker_df, tmp_df)
}

#americanthinker_df2 comes from same code above but with novemever as month
americanthinker_final <- rbind(americanthinker_df, americanthinker_df2) 

americanthinker_final <- americanthinker_final %>% 
  mutate(date = as.Date(date, format = "%B %d, %Y"))

write_rds(americanthinker_final, paste0(data_path, "americanthinker_df.rds"))
