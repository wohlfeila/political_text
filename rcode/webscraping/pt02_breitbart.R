# creating vector of days in 2019
dates_loops <- c(2,3,12,18,23,27)
months_loops <- c(8, 9, 10, 11, 12)

# creating empty vector
breitbart_df <- c()

for (i in dates_loops){
  for (j in months_loops){
    # pulling in first day
    day <- i
    month <- j
    
    # creating url
    url <- paste0("https://www.breitbart.com/politics/2019/",month,"/", day, "/")
    
    # reading url
    webpage <- read_html(url)
    
    # pulling in blog post links
    links <- webpage %>% 
      html_nodes("h2 a") %>% 
      html_attr("href")
    
    links <- links[!grepl("clips", links)]
    
    for (j in 1:length(links)){
      
      # reading each link
      read_link <- paste0("https://www.breitbart.com",links[[j]], collapse = "")  
      
      # creating error catching
      tryCatch(
        # reading html link
        breitbart_html <- read_html(read_link),
        error = function(e) {NA})
      
      # reading text from link
      breitbart_text <- breitbart_html %>% 
        html_nodes("div p") 
      
      # removing nodes that do not correspond to actual text
      breitbart_text <- breitbart_text[1:(length(breitbart_text)-5)]
      
      # writing to text
      breitbart_text <- breitbart_text %>% 
        html_text()
      
      breitbart_text <- sub("(pic.twitter[^ ]*)","",breitbart_text)
      breitbart_text <- sub("(http[^ ]*)","",breitbart_text)
      breitbart_text <- breitbart_text[!grepl("^— ", breitbart_text)]
      
      # collapsing text
      breitbart_text <- paste0(breitbart_text, collapse = " ")
      
      # reading title nodes
      breitbart_title <- breitbart_html %>% 
        html_nodes("h1") %>% 
        html_text()
      
      # reading tag nodes
      breitbart_tags <- breitbart_html %>% 
        html_nodes("a") %>% 
        html_attr("href")
      
      breitbart_tags <- breitbart_tags[50:length(breitbart_tags)]  
      breitbart_tags <- breitbart_tags[grepl("/tag/", breitbart_tags)]
      breitbart_tags <- gsub(".*/(.*)\\/.*", "\\1", breitbart_tags)
      breitbart_tags <- paste0(breitbart_tags, collapse = ";")
      
      # reading author nodes
      author <- breitbart_html %>% 
        html_nodes("address a") %>% 
        html_text()
      
      # reding date nodes
      date <- breitbart_html %>% 
        html_nodes("time") %>% 
        html_text()
      
      # creating data.frame from scraped data
      tmp_df <- data.frame(date = date, 
                           blog = "Breitbart", 
                           author = author, 
                           title = breitbart_title, 
                           text = breitbart_text,
                           tags = breitbart_tags)
      
      # appednding rows of new scraped data to data.frame
      breitbart_df <- rbind(breitbart_df, tmp_df)
    }
  }
}  
 
# re-formatting dates and removing duplicate rows
breitbart_df <- taRifx::unfactor.data.frame(breitbart_df) %>% 
  dplyr::distinct(title, .keep_all = TRUE) %>% 
  mutate(date = as.Date(date, '%d %b %Y'),
         title = tolower(title)) %>% 
  filter(!grepl("watch:", title) | text != "")

# writing out data.frame
write_csv(breitbart_df, paste0(data_path,"breitbart_df_2019.csv"))