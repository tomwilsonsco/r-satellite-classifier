source("R/load_libraries.R")
source("R/training_data.R")
source("R/train_test_rf.R")
source("R/predict_rf.R")

# Read files --------------------------------------------------------------

tif_file <- list.files("data", "*.tif")[1]
tif_file_path <- file.path("data", tif_file)

test_raster <- brick(tif_file_path)

gpkg_file <- list.files("data", ".gpkg")[1]
gpkg_file_path <- file.path("data", gpkg_file)

test_training_samples <- read_sf(gpkg_file_path)


# Build training set ------------------------------------------------------

training_df <- get_pixels_per_class(test_raster, test_training_samples, "ml_class")
# Optionally balance classes
training_df <- balance_classes(training_df, "ml_class")


# Train-test split and test RF --------------------------------------------

test_split_data <- train_test_split(training_df, class_column = "ml_class")
train_test_rf(training_df, "ml_class", n_tests = 5, n_tree = 10)


# Train rf using all training data ----------------------------------------

rf <- train_rf(training_df, "ml_class")


# Predict whole image classified and probability --------------------------

# Classified
predict_image_rf(tif_file_path, pred_type = "response")
# Probability
predict_image_rf(tif_file_path, pred_type = "prob")