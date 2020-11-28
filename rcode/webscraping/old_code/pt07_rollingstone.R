url <- paste0("https://www.rollingstone.com/politics/page/2/")
webpage <- read_html(url)

links <- webpage %>% 
  html_nodes("a.c-card__wrap") %>% 
  html_attr("href")

links <- links[grepl("https://www.rollingstone.com/", links)]  

rollingstone_html <- read_html(links[[2]])

rollingstone_text <- rollingstone_html %>% 
  html_nodes("div p") 

rollingstone_text <- rollingstone_text %>% 
  html_text() 

rollingstone_text <- rollingstone_text[8:length(rollingstone_text) - 8]
rollingstone_text <- rollingstone_text[8:length(rollingstone_text)]

tmp <- gsub("[\r\t]", "", rollingstone_text)
tmp <- trimws(tmp)
tmp <- paste0(tmp, collapse = " ")

