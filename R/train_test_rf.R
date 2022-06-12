# shuffle function
shuffle_df <- function(input_df) {
  shuffled_rows <- sample(nrow(input_df))
  input_df[shuffled_rows, ]
}

train_test_split <- function(sample_df, class_column = "ml_class", train_prop = 0.75) {
  sample_df$row_id <- 1:nrow(sample_df)
  sample_df[[class_column]] <- as.factor(sample_df[[class_column]])
  train_df <- sample_df %>% dplyr::slice_sample(prop = train_prop)
  test_df <- sample_df %>% dplyr::anti_join(train_df, by = "row_id")
  train_df <- train_df %>%
    select(-row_id, -ID) %>%
    shuffle_df()
  test_df <- test_df %>%
    select(-row_id, -ID) %>%
    shuffle_df()
  train_x <- train_df %>% select(-.data[[class_column]])
  train_y <- train_df %>% select(.data[[class_column]])
  test_x <- test_df %>% select(-.data[[class_column]])
  test_y <- test_df %>% select(.data[[class_column]])
  list(train_x = train_x, train_y = train_y, test_x = test_x, test_y = test_y)
}

train_test_rf <- function(sample_df, class_column, n_tests = 5, n_tree = 100) {
  for (i in 1:n_tests) {
    split_data <- train_test_split(sample_df, class_column = class_column)
    rf <- randomForest(
      x = split_data$train_x,
      y = split_data$train_y[[class_column]],
      xtest = split_data$test_x,
      ytest = split_data$test_y[[class_column]],
      ntree = n_tree
    )
    print(paste("test", i, ":"))
    print(rf$test$confusion)
    rm(rf)
  }
}

train_rf <- function(sample_df, class_column, n_tree = 100, out_file_prefix = "random_forest") {
  sample_df[[class_column]] <- as.factor(sample_df[[class_column]])
  sample_df <- shuffle_df(sample_df)
  train_x <- sample_df %>% select(-ID, -.data[[class_column]])
  print(head(train_x))
  train_y <- sample_df[[class_column]]
  rf <- randomForest(x = train_x, y = train_y, ntree = n_tree)
  out_file <- paste0("models/", out_file_prefix, Sys.Date(), ".rds")
  saveRDS(rf, file = out_file)
  print(paste("saved trained model to", out_file))
  return(rf)
}