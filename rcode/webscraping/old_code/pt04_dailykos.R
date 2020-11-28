# creating url to pull data from
url <- paste0("https://www.dailykos.com/user/main/history")

# reading url
webpage <- read_html(url)

# pulling links
links <- webpage %>% 
  html_nodes("td a") %>% 
  html_attr("href")

# removing nodes with 'user'
links <- links[!grepl("user",links)]

# removing nodes with 'accounts'
links <- links[!grepl("/accounts",links)]

# removing nodes with 'cartoon' - error when parsing an image - no text
links <- links[!grepl("Cartoon",links)]

# removing nodes with 'Radio' - could not parse radio player
links <- links[!grepl("Daily-Kos-Radio",links)]


# creating empty vector
dailykos_df <- c()

for (i in 1:length(links)){
  
  # creating url
  dailykos_url <- paste0("https://www.dailykos.com",links[[i]])
  
  # reading url
  dailykos_html <- read_html(dailykos_url)
  
  # pulling text nodes and converting into text
  dailykos_text <- dailykos_html %>% 
    html_nodes("div p") %>% 
    html_text()
  
  # collapsing text to single string
  dailykos_text <- paste0(dailykos_text, collapse = " ")
  
  # pulling date text
  date <- dailykos_html %>% 
    html_nodes(".timestamp") %>% 
    html_text()
  
  # re-formatting date
  date <- as.Date(date[3], '%Y/%m/%d')
  
  # pulling author text
  author <- dailykos_html %>% 
    html_nodes("span a") %>% 
    html_text()
  
  # chosing correct element that contains author
  author <- author[1]
  
  # creating data.frame with scraped data
  tmp_df <- data.frame(date = date, blog = "Daily Kos", author = author, text = dailykos_text)
  
  # appending new rows to data.frame
  dailykos_df <- rbind(dailykos_df, tmp_df)
  
}

# unfactor data.frame
dailykos_df <- taRifx::unfactor.data.frame(dailykos_df)

# pulling all titles of blogs
dailykos_title <- webpage %>% 
  html_nodes("td a")

# removing author nodes
dailykos_title <- dailykos_title[!grepl("author", dailykos_title)]

# last element is not a proper title
dailykos_title <- dailykos_title[-1]

# removing everything after last /
dailykos_title <- sub(".*/-", "", dailykos_title)

# removing last 4 characters '</a>'
dailykos_title <- gsub('.{4}$', '', dailykos_title)

# combining titles with data.frame with other data
dailykos_df <- cbind(dailykos_df, title = dailykos_title[1:nrow(dailykos_df)])

# re-ordering columns so combining data.frames in data prep step is easier
dailykos_df <- dailykos_df %>% 
  select(date, blog, author, title, text)

# writing our data  
write_rds(dailykos_df, paste0(data_path,"dailykos_df_2.rds"))
