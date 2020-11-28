url <- paste0("https://slate.com/news-and-politics/politics")

url <- paste0("https://slate.com/news-and-politics/politics/2")
webpage <- read_html(url)

links <- webpage %>% 
  html_nodes("div a") %>% 
  html_attr("href") 

links <- links[grepl("https://slate.com/news-and-politics/2019", links)]  

slate_html <- read_html(links[[1]])

slate_text <- slate_html %>% 
  html_nodes("div p") 

slate_text <- slate_text[grepl("text", slate_text)]

slate_text <- slate_text %>% 
  html_text() 

tmp <- gsub("[\r\n]", "", slate_text)
tmp <- trimws(tmp)
tmp <- paste0(tmp, collapse = " ")

