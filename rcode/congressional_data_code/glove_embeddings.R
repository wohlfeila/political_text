library(keras)
library(tidyverse)
library(glue)
library(progress)

# create file paths
data_file <- "/Volumes/TOSHIBA EXT/gitrepo_data/"
glove_path <- "data/glove.6B/glove.6B.100d.txt"

# read in glove weights
glove_wts <- data.table::fread(glove_path, quote = "", data.table = FALSE) %>% 
  as_tibble()

# read in test/train data
congressional_text_data <- read_rds(paste0(data_file, "congressional_text_data_stop_words_removed.rds"))

c(train_text, train_labels, test_text, test_labels) %<-% congressional_text_data

# calculating baseline for model performance - guess every speech is from a democrat
baseline <- sum(train_labels) / length(train_labels)

# choosing top words to be considering in tokenization  
top_n_words <- 10000

# fitting tokenizer
tokenizer <- text_tokenizer(num_words = top_n_words) %>% 
  fit_text_tokenizer(train_text)

# creating word index
total_word_index <- tokenizer$word_index

# looking at number of words used
num_words_used <- tokenizer$num_words

# creating sequences for text to use in embeddings
sequences <- texts_to_sequences(tokenizer, train_text)

# choosing max length of input sequence 
max_len <- 150

# creating uniform feature set
features <- pad_sequences(sequences,  padding = "post", maxlen = max_len)

# creating word index to compare to GloVe
applicable_index <- total_word_index[total_word_index <= top_n_words]

# applying names
applicable_words <- names(applicable_index)

# seeing how many words within our text will have GloVe embeddings
available_wts <- glove_wts %>%
  filter(V1 %in% applicable_words) %>% 
  pull(V1)

diff <- length(applicable_words) - length(available_wts)

glue("There are {diff} words in our Congressional data that are not represented in GloVe")

# required dimensions of our embedding matrix
num_words_used <- length(applicable_words)

# creating GloVe matrix
embedding_dim <- ncol(glove_wts) - 1

# create empty matrix
embedding_matrix <- matrix(0, nrow = num_words_used, ncol = embedding_dim)

row.names(embedding_matrix) <- applicable_words

# first 10 rows & columns of our empty matrix
embedding_matrix[1:10, 1:10]

# this just allows us to track progress of our loop
pb <- progress_bar$new(total = num_words_used)

for (word in applicable_words) {
  # track progress
  pb$tick()
  
  # get embeddings for a given word
  embeddings <- glove_wts %>%
    filter(V1 == word) %>%
    select(-V1) %>% 
    as.numeric()
  
  # if embeddings don't exist create a vector of all zeros
  if (all(is.na(embeddings))) {
    embeddings <- vector("numeric", embedding_dim)
  }
  
  # add embeddings to appropriate location in matrix
  embedding_matrix[word, ] <- embeddings
}

#### modeling ----

# building model architecture 
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = top_n_words, 
                  output_dim = embedding_dim, 
                  input_length = max_len) %>% 
  layer_flatten() %>%
  layer_dense(units = 1, activation = "sigmoid")

# freezing GloVe weights as first layer
get_layer(model, index = 1) %>% 
  set_weights(list(embedding_matrix)) %>% 
  freeze_weights()

summary(model)

# setting hyperparameters
model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = "binary_crossentropy",
  metrics = c("acc")
)

model_callbacks <- list(
  callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE),
  callback_reduce_lr_on_plateau(patience = 2, min_lr = 0.00001)
)

# fitting the model
history <- model %>% fit(
  features, train_labels,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = model_callbacks
)

plot(history)

best_epoch <- which(history$metrics$val_loss == min(history$metrics$val_loss))
loss <- history$metrics$val_loss[best_epoch] %>% round(3)
acc <- history$metrics$val_acc[best_epoch] %>% round(3)
glue("The best epoch had a loss of {loss} and accuracy of {acc}")

# evaluating test set
sequences_test <- texts_to_sequences(tokenizer, test_text)
test_features <- pad_sequences(sequences_test, maxlen = 150) 

results <- model %>% evaluate(test_features, test_labels)

# That means :
# - a low accuracy and huge loss means you made huge errors on a lot of data
# - a low accuracy but low loss means you made little errors on a lot of data
# - a great accuracy with low loss means you made low errors on a few data (best case)
# - your situation : a great accuracy but a huge loss, means you made huge errors on a few data.

# saving model ----
object_path <- "/Volumes/TOSHIBA EXT/gitrepo_data/shiny_app_classification_objects/"
model %>% save_model_hdf5(paste0(object_path,"glove_base_model"))

# saving tokenizer
save_text_tokenizer(tokenizer, paste0(object_path,"congressional_tokenizer"))

# shiny process - republican speaker
new_model <- load_model_hdf5(paste0(object_path,"glove_base_model"))
summary(new_model)
tmp_tokenizer <- load_text_tokenizer(paste0(object_path,"congressional_tokenizer"))

shiny_text <- "This is a big deal and it's going to make a significant difference for us. Also, the state of Oregon contacted us and is going to send 140 ventilators, Cuomo added. Which is I can tell you just astonishing and unexpected. And I want to thank Gov. Brown, I want to thank all of the people in the state of Oregon for their thoughtfulness."

shiny_text <- tolower(shiny_text)

shiny_sequence <- texts_to_sequences(tmp_tokenizer, shiny_text)

# padding sequences
shiny_features <- pad_sequences(shiny_sequence, padding = "post", maxlen = 150) 

cat(crayon::blue("Original text:\n"))
shiny_text[[1]]
cat(crayon::blue("Sequence:\n")) # converting words into numbers using the tokenizer
shiny_sequence[[1]]
cat(crayon::blue("\nRevised text:\n")) # only keeping words in tokenizer
paste(unlist(tmp_tokenizer$index_word)[shiny_sequence[[1]]] , collapse = " ")
cat(crayon::blue("\nEncoded text:\n")) # filling in zeros so each speech is length 150
shiny_features[1,]

prediction <- predict_classes(object = new_model, x = shiny_features)

