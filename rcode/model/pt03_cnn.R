# sourcing enviornment files
source("rcode/enviornmentsetup/pt01_functions.R")
source("rcode/enviornmentsetup/pt02_environment_configuration.R")

# read in test/train data
blog_text_data <- read_rds(paste0(blog_data_path, "blog_text_data.rds"))

c(train_text, train_labels, test_text, test_labels) %<-% blog_text_data

# building out text input -----------------------------------------------------
# choosing top words to be considering in tokenization  
top_n_words_text <- 15000

# fitting tokenizer
tokenizer_text <- text_tokenizer(num_words = top_n_words_text) %>% 
  fit_text_tokenizer(train_text)

# creating sequences for text to use in embeddings
sequences_text <- texts_to_sequences(tokenizer_text, train_text)

# choosing max length of input sequence 
max_len <- 512

# creating uniform feature set
features_text <- pad_sequences(sequences_text,  padding = "post", maxlen = max_len)

embedding_dim <- 512

# building model architecture ------------------------------------------

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = top_n_words_text, 
                 output_dim = embedding_dim, 
                 input_length = max_len) %>% 
  layer_conv_1d(filters = 256, kernel_size = 3) %>% 
  layer_average_pooling_1d(pool_size = 3) %>% 
  layer_dropout(0.3) %>% 
  layer_flatten() %>% 
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

# setting hyperparameters
model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

model_callbacks <- list(
  callback_early_stopping(monitor = "val_loss", 
                          patience = 5, 
                          restore_best_weights = TRUE),
  callback_reduce_lr_on_plateau(patience = 2, 
                                min_lr = 0.0001)
)

# fitting the model
history <- model %>% fit(
  features_text, 
  train_labels,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = model_callbacks
)

plot(history)
