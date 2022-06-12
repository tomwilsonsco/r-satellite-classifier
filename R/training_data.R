# Extract pixel values from training------------------------------
get_pixels_per_class <- function(input_raster, training_shapes, class_column) {
  if (!compareCRS(input_raster, training_shapes)) {
    stop("raster and training shapes not in same CRS!!")
  }
  pixels_all <- list()
  distinct_classes <- unique(training_shapes[[class_column]])
  for (i in distinct_classes) {
    class_shapes <- training_shapes %>% dplyr::filter(.data[[class_column]] == i)
    class_pixels <- raster::extract(input_raster, class_shapes, df = TRUE)
    class_pixels$ml_class <- i
    pixels_all[[i + 1]] <- class_pixels
  }
  dplyr::bind_rows(pixels_all)
}

# Balance class samples-------------------------------------------
# simple count function
count_per_class <- function(sample_df, class_column) {
  group_count <- sample_df %>%
    dplyr::group_by(.data[[class_column]]) %>%
    dplyr::summarise(sample_count = n())
  print(group_count)
  return(group_count)
}

# Balance function
balance_classes <- function(sample_df, class_column) {
  group_count <- count_per_class(sample_df, class_column)
  distinct_classes <- unique(group_count[[class_column]])
  smallest_sample <- min(group_count[["sample_count"]])
  resampled_results <- list()
  for (i in distinct_classes) {
    class_pixels <- sample_df %>% dplyr::filter(.data[[class_column]] == i)
    class_pixels <- dplyr::slice_sample(class_pixels, n = smallest_sample)
    resampled_results[[i + 1]] <- class_pixels
  }
  resampled_df <- dplyr::bind_rows(resampled_results)
  # To check difference from originally
  count_per_class(resampled_df, class_column)
  return(resampled_df)
}

