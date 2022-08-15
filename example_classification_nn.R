library(neuralnet)
library(dplyr)
library(stringr)

# Make 2 class subset of Iris dataset
iris_test <- iris %>% filter(Species !="virginica") %>% as_tibble()

# Make the column names tidy
colnames(iris_test) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "species")

# Create the ml class column
iris_test <- iris_test %>% mutate( ml_class = case_when(species=="setosa"~1,
                                            species=="versicolor"~0))

# Get scaler vals per column (mean, std for centering data)
get_scaler <- function(input_df, scale_columns){
  input_df %>% select(all_of(scale_columns)) %>% summarise_all(list(mean=mean, sd=sd))
}

# Apply the scaler to dataset
apply_scaler <- function(input_df, scaler){
  scale_columns <- colnames(scaler) %>% str_replace_all("(_mean|_sd)", "") %>% unique()
  for (col in scale_columns){
    scale_mean <- scaler[[paste0(col, "_mean")]]
    scale_sd <- scaler[[paste0(col, "_sd")]]
    input_df[[col]]= (input_df[[col]] - scale_mean) / scale_sd
  }
  return(input_df)
}

# Stratified train-test split
stratified_train_test <- function(input_df, train_prop=0.7){
  input_df <- input_df %>% mutate(row_id = row_number())
  distinct_classes <- input_df %>% select(ml_class) %>% unique() %>% pull()
  train_l <- list()
  test_l <- list()
  for (c in distinct_classes){
    train_df <- input_df %>% filter(ml_class==c) %>% slice_sample(prop=train_prop)
    test_df <- input_df %>% filter(ml_class==c) %>% anti_join(train_df, by="row_id")
    train_l[[as.character(c)]] <-train_df
    test_l[[as.character(c)]]<- test_df
    
  }
 train <- bind_rows(train_l)
 test <- bind_rows(test_l)
  return(list(train=train, test=test))
}

# Train test the Iris subset
train_test_list <- stratified_train_test(iris_test)

# Get the train / test dfs from the returned list
train <- train_test_list[["train"]]
test <- train_test_list[["test"]]

# Get namees of the columns requiring scaling (i.e. the X columns model trained on)
scale_columns <- colnames(iris_test)[!colnames(train) %in% c("row_id", "species", "ml_class")]

# Get the scaler made from just training set
scaler <- get_scaler(train, scale_columns)

# Apply the scaler to train and test
train <- apply_scaler(train, scaler)

test <- apply_scaler(test, scaler)

# Create a neuralnet::neuralnet nn object
nn <- neuralnet(ml_class~sepal_length+sepal_width+petal_length+petal_width, data=train, hidden=3)
# Plot the network structure with trained weights
plot(nn)

# Make predictions on the test set and convert from prob to class
pred_test <- test %>% mutate(pred=predict(nn, test)) %>% mutate(pred=ifelse(pred < 0.5, 0, 1))

# Calculate an overall accuracy %
nrow(pred_test %>% filter(ml_class==pred)) / nrow(test) * 100

