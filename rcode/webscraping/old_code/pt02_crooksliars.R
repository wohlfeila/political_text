url <- paste0("https://crooksandliars.com/politics?page=1")

webpage <- read_html(url)

links <- webpage %>% 
  html_nodes("h2 a") %>% 
  html_attr("href")
  
links <- links[!grepl("cltv", links)]

cl_df <- c()

for (i in 1:length(links)){
  cl_html <- read_html(paste0("https://crooksandliars.com/", links[i]))
  
  cl_text <- cl_html %>% 
    html_nodes("div p")
  
  cl_text_2 <- cl_text[1:(length(cl_text)-3)]
  
  cl_text_2 <- cl_text_2 %>%
    html_text()
  
  cl_text_2 <- paste0(cl_text_2, collapse = " ")
  
  tmp_df <- data.frame(blog = "Crooks and Liars", text = cl_text_2, label = "liberal")
  
  # appending new rows to data.frame
  cl_df <- rbind(cl_df, tmp_df)
} 

write_rds(cl_df, paste0(data_path,"cl_df.rds"))


