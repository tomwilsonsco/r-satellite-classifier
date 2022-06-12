find_read_model <- function(folder_name, file_ext, trained_rf = NULL) {
  if (is.null(trained_rf)) {
    all_files <- list.files(path = folder_name, pattern = paste0("*.", file_ext))
    most_recent <- tail(sort(all_files), n = 1)
    rf_file_path <- file.path(folder_name, most_recent)
  } else {
    rf_file_path <- file.path(folder_name, trained_rf)
  }
  rf <- readRDS(rf_file_path)
  return(rf)
}

create_output_fp <- function(source_raster_fp, pred_type) {
  source_dir <- dirname(source_raster_fp)
  dir.create(file.path(source_dir, "prediction_images"), showWarnings = FALSE)
  out_dir <- file.path(source_dir, "prediction_images")
  source_file <- basename(source_raster_fp)
  if (pred_type == "response") {
    out_file <- paste0("classify_", source_file)
  } else if (pred_type == "prob") {
    out_file <- paste0("prob_", source_file)
  }
  file.path(out_dir, out_file)
}

predict_image_rf <- function(source_raster_fp, pred_type = "response", trained_rf = NULL) {
  # get model
  rf_classifier <- find_read_model("models", "rds", trained_rf)
  # get out file
  out_raster_fp <- create_output_fp(source_raster_fp, pred_type)
  # raster input output variables
  source_raster <- raster::brick(source_raster_fp)
  out_raster <- raster::raster(source_raster_fp)
  block_data <- raster::blockSize(source_raster)
  print(paste("processing in", block_data$n, "blocks of", block_data$nrows[1], "rows..."))
  # read write for block processing
  source_read <- raster::readStart(source_raster)
  out_write <- raster::writeStart(out_raster, filename = out_raster_fp, overwrite = TRUE)
  # loop through blocks
  for (i in seq_along(block_data$row)) {
    row_vals <- raster::getValuesBlock(source_raster, row = block_data$row[i], nrows = block_data$nrows[i])
    row_pred <- randomForest:::predict.randomForest(newdata = row_vals, object = rf_classifier, type = pred_type, factors = names(row_vals))
    if (pred_type == "response") {
      row_pred <- as.numeric(row_pred)
      # rf predicts training class 0 as 1, training class 1 as 2, so correct thi by subtracting 1:
      row_pred <- row_pred - 1
      #if a prob prediction take 2nd value for each cell
    } else if (pred_type == "prob") {
      row_pred <- row_pred[, 2]
    }
    row_pred <- matrix(row_pred, nrow = block_data$nrows[i])
    out_write <- raster::writeValues(x = out_write, v = row_pred, start = block_data$row[i])
    print(paste("processed block", i, "of", block_data$n[1]))
  }
  out_write <- raster::writeStop(out_write)
  source_read <- raster::readStop(source_read)
  print(paste("prediction image written to", out_raster_fp))
}