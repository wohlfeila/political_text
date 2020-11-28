url <- paste0("http://michellemalkin.com/2019/12/")

webpage <- read_html(url)

links <- webpage %>% 
  html_nodes("h2 a") %>% 
  html_attr("href")

mm_df <- c()

for (i in 1:length(links)){
  mm_html <- read_html(links[i])
  
  mm_text <- mm_html %>% 
    html_nodes("div p")
  
  mm_text <- mm_text[!grepl("style", mm_text)]
  mm_text <- mm_text[!grepl("a href", mm_text)]
  mm_text <- mm_text[-1]
  mm_text <- mm_text[1:(length(mm_text)-6)]
  mm_text <- mm_text %>% 
    html_text()
  mm_text <- paste0(mm_text, collapse = " ")
  
  tmp_df <- data.frame(blog = "Michelle Malkin", text = mm_text, label = "conservative")
  
  # appending new rows to data.frame
  mm_df <- rbind(mm_df, tmp_df)
} 

write_rds(mm_df, paste0(data_path,"mm_df.rds"))
