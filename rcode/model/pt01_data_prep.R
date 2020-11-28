# sourcing enviornment files
source("rcode/enviornmentsetup/pt01_functions.R")
source("rcode/enviornmentsetup/pt02_environment_configuration.R")

# getting data files
files <- list.files(blog_data_path, full.names = TRUE)
files <- files[grepl(".csv", files)]

# loading blog csv files
file_load <- lapply(files, function(x) read_csv(x, col_types = cols(
  date = col_date(format = ""),
  blog = col_character(),
  author = col_character(),
  title = col_character(),
  text = col_character(),
  tags = col_character()
  )))

blog_files <- do.call(rbind, file_load)

# initial data cleaning --------------------------------------------------------
blog_files <- blog_files %>% 
  filter(!is.na(date)) %>% 
  mutate(author = replace_na(author, "no author"),
         title = tolower(title),
         text = tolower(text),
         classification_label = if_else(blog == "Mother Jones", 1, 0)) %>% 
  filter(!grepl("watch", title)) 

blog_files <- tibble::rowid_to_column(blog_files, "id")

# data check - unique id for each row in data.frame
length(unique(blog_files$id)) == nrow(blog_files)

# creating labels --------------------------------------------------------------
blog_tags <- str_split(blog_files$tags, ";")
names(blog_tags) <- blog_files$id

# creating label matrix
labels <- tabList(blog_tags)

# cleaning labels - reducing number of labels
label_count <- colSums(labels)
row_count <- rowSums(labels)

# model data prep -------------------------------------------------------------
model_blog_data <- blog_files %>% 
  select(-tags)

# write out data for EDA
write_rds(model_blog_data, paste0(blog_data_path, "blog_ed_data.rds"))

# train/test split - taking a sample and randomizing data
set.seed(42)
rows <- sample(nrow(model_blog_data))
model_data <- model_blog_data[rows, ]

train_index <- sample.int(n = nrow(model_data), size = floor(0.80 * nrow(model_data)), replace = F)

# creating test/train data sets
train_text <- model_data$text[train_index]
test_text <- model_data$text[-train_index]
train_labels <- model_data$classification_label[train_index]
test_labels <- model_data$classification_label[-train_index]

blog_text_data <- list(train_text, train_labels, test_text, test_labels)

write_rds(blog_text_data, paste0(blog_data_path, "blog_text_data.rds"))
