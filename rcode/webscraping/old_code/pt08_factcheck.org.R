#scraping page one ----
month <- seq(1, 12, 1)

factcheck.org_df <- c()

for (i in month){
  url <- paste0("https://www.factcheck.org/2019/",i, "/")
  webpage <- read_html(url)
  
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # creating empty vecotr
  month_df <- c()
  
  for(j in 1:length(links)){
    
    factcheck_html <- read_html(links[j])
    
    factcheck_text <- factcheck_html %>% 
      html_nodes("div div") %>% 
      html_nodes("div p") %>% 
      html_text()
    
    factcheck_text <- factcheck_text[3:length(factcheck_text)]
    factcheck_text <- paste0(factcheck_text, collapse = " ")
    factcheck_text <- sub("Editor’s note:.*", "", factcheck_text) 
    
    title <- factcheck_html %>% 
      html_nodes("header h1") %>% 
      html_text()
    
    author <- factcheck_html %>% 
      html_nodes("p a") %>% 
      html_text()
    
    author <- author[1]
    
    date <- factcheck_html %>% 
      html_nodes("p time") %>% 
      html_text()
    
    # building data.frame from scraped data
    tmp_df <- data.frame(date = date, blog = "Factcheck.org", author = author, title = title, text = factcheck_text)
    
    # appending data to existing data.frame
    month_df <- rbind(month_df, tmp_df)
  }
  factcheck.org_df <- rbind(factcheck.org_df, month_df)
}

# scraping additional pages ----
pages <- "page/2/"
factcheck.org_df2 <- c()

for (i in month){
  url <- paste0("https://www.factcheck.org/2019/",i, "/", pages)
  webpage <- read_html(url)
  
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # creating empty vecotr
  month_df <- c()
  
  for(j in 1:length(links)){
    
    factcheck_html <- read_html(links[j])
    
    factcheck_text <- factcheck_html %>% 
      html_nodes("div div") %>% 
      html_nodes("div p") %>% 
      html_text()
    
    factcheck_text <- factcheck_text[3:length(factcheck_text)]
    factcheck_text <- paste0(factcheck_text, collapse = " ")
    factcheck_text <- sub("Editor’s note:.*", "", factcheck_text) 
    
    title <- factcheck_html %>% 
      html_nodes("header h1") %>% 
      html_text()
    
    author <- factcheck_html %>% 
      html_nodes("p a") %>% 
      html_text()
    
    author <- author[1]
    
    date <- factcheck_html %>% 
      html_nodes("p time") %>% 
      html_text()
    
    # building data.frame from scraped data
    tmp_df <- data.frame(date = date, blog = "Factcheck.org", author = author, title = title, text = factcheck_text)
    
    # appending data to existing data.frame
    month_df <- rbind(month_df, tmp_df)
  }
  factcheck.org_df2 <- rbind(factcheck.org_df2, month_df)
}


# scraping additional pages ----
pages <- "page/3/"
factcheck.org_df3 <- c()

for (i in month){
  url <- paste0("https://www.factcheck.org/2019/",i, "/", pages)
  webpage <- read_html(url)
  
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # creating empty vecotr
  month_df <- c()
  
  for(j in 1:length(links)){
    
    factcheck_html <- read_html(links[j])
    
    factcheck_text <- factcheck_html %>% 
      html_nodes("div div") %>% 
      html_nodes("div p") %>% 
      html_text()
    
    factcheck_text <- factcheck_text[3:length(factcheck_text)]
    factcheck_text <- paste0(factcheck_text, collapse = " ")
    factcheck_text <- sub("Editor’s note:.*", "", factcheck_text) 
    
    title <- factcheck_html %>% 
      html_nodes("header h1") %>% 
      html_text()
    
    author <- factcheck_html %>% 
      html_nodes("p a") %>% 
      html_text()
    
    author <- author[1]
    
    date <- factcheck_html %>% 
      html_nodes("p time") %>% 
      html_text()
    
    # building data.frame from scraped data
    tmp_df <- data.frame(date = date, blog = "Factcheck.org", author = author, title = title, text = factcheck_text)
    
    # appending data to existing data.frame
    month_df <- rbind(month_df, tmp_df)
  }
  factcheck.org_df3 <- rbind(factcheck.org_df3, month_df)
}



# scraping additional pages ----
pages <- "page/4/"
factcheck.org_df4 <- c()

for (i in month){
  url <- paste0("https://www.factcheck.org/2019/",i, "/", pages)
  webpage <- read_html(url)
  
  links <- webpage %>% 
    html_nodes("h3 a") %>% 
    html_attr("href")
  
  # creating empty vecotr
  month_df <- c()
  
  for(j in 1:length(links)){
    
    factcheck_html <- read_html(links[j])
    
    factcheck_text <- factcheck_html %>% 
      html_nodes("div div") %>% 
      html_nodes("div p") %>% 
      html_text()
    
    factcheck_text <- factcheck_text[3:length(factcheck_text)]
    factcheck_text <- paste0(factcheck_text, collapse = " ")
    factcheck_text <- sub("Editor’s note:.*", "", factcheck_text) 
    
    title <- factcheck_html %>% 
      html_nodes("header h1") %>% 
      html_text()
    
    author <- factcheck_html %>% 
      html_nodes("p a") %>% 
      html_text()
    
    author <- author[1]
    
    date <- factcheck_html %>% 
      html_nodes("p time") %>% 
      html_text()
    
    # building data.frame from scraped data
    tmp_df <- data.frame(date = date, blog = "Factcheck.org", author = author, title = title, text = factcheck_text)
    
    # appending data to existing data.frame
    month_df <- rbind(month_df, tmp_df)
  }
  factcheck.org_df4 <- rbind(factcheck.org_df4, month_df)
}

# combining texts ----

factcheck.org_final <- rbind(factcheck.org_df, factcheck.org_df2, factcheck.org_df3, factcheck.org_df4)

factcheck.org_final <- factcheck.org_final %>% 
  mutate(text = sub("Q: ","",factcheck.org_final$text),
         text = sub("A: ","",factcheck.org_final$text),
         date = as.Date(date, format = "%B %d, %Y"))

write_rds(factcheck.org_final, paste0(data_path, "factcheck.org_df.rds"))
