# sourcing enviornment files
source("rcode/enviornmentsetup/pt01_functions.R")
source("rcode/enviornmentsetup/pt02_environment_configuration.R")

#library(udpipe)

# read in test/train data
blog_text_data <- read_rds(paste0(blog_data_path, "blog_text_data.rds"))

c(train_text, train_labels, test_text, test_labels) %<-% blog_text_data

#parse_text <- tibble(doc_id = seq(1, length(train_text), 1), text = train_text)

#pos_tag <- udpipe(parse_text, object = "english")

#write_rds(pos_tag, paste0(blog_data_path, "pos_tag.rds"))

pos_tag <- read_rds(paste0(blog_data_path, "pos_tag.rds"))

length(unique(pos_tag$doc_id)) == length(train_text)

pos_combine <- pos_tag %>% 
  dplyr::group_by(doc_id) %>% 
  dplyr::summarise(pos = paste0(upos, collapse = " ")) %>% 
  ungroup() %>% 
  mutate(doc_id = as.numeric(sub("doc", "", doc_id))) %>% 
  arrange(doc_id)

median_text_plot <- function(text){
  text_df <- text %>%
    tibble(.name_repair = ~ "text") %>%
    mutate(text_length = str_count(text, "\\w+"))
  
  unique_words <- text_df %>%
    tidytext::unnest_tokens(word, text) %>%
    pull(word) %>%
    n_distinct()
  
  median_review_length <- median(text_df$text_length, na.rm = TRUE)
  
  ggplot(text_df, aes(text_length)) +
    geom_histogram(bins = 100, fill = "grey70", color = "grey40") +
    geom_vline(xintercept = median_review_length, color = "red", lty = "dashed") +
    scale_x_log10("# words") +
    ggtitle(glue("Median document length is {median_review_length} words"),
            subtitle = glue("Total number of unique words is {unique_words}"))
}

median_text_plot(train_text)
median_text_plot(pos_combine$pos)

# choosing top words to be considering in tokenization  
top_n_words <- 17

# fitting tokenizer
tokenizer <- text_tokenizer(num_words = top_n_words) %>% 
  fit_text_tokenizer(pos_combine$pos)

# creating sequences for text to use in embeddings
sequences <- texts_to_sequences(tokenizer, pos_combine$pos)

# choosing max length of input sequence 
max_len <- 512

# creating uniform feature set
features <- pad_sequences(sequences,  padding = "post", maxlen = max_len)

text_input <- layer_input(shape = c(512, 1))
pos_input <- layer_input(shape = c(512, 1))

encoded_channel_one <- text_input %>% 
  layer_conv_1d(filters = 256, kernel_size = 3) %>% 
  layer_max_pooling_1d(pool_size = 3) %>% 
  layer_dropout(0.3) 

encoded_channel_two <- pos_input %>% 
  layer_conv_1d(filters = 256, kernel_size = 3) %>% 
  layer_max_pooling_1d(pool_size = 3) %>% 
  layer_dropout(0.3) 

predictions <- layer_concatenate(c(encoded_channel_one, encoded_channel_two), axis=-1) %>% 
  layer_dense(units = 1, activation = 'sigmoid')

model <- keras_model(inputs = c(text_input, pos_input), outputs = predictions)

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

summary(model)

model %>% fit(list(data_a, data_b), labels, epochs = 10)