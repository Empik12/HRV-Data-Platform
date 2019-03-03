saveData <- function(data, file_name) {
  # Create a temporary file to hold the data
  data <- t(data)
  file_path <- file.path(tempdir(), paste(file_name,".csv"))
  write.csv(data ,file_path, row.names = FALSE, quote = TRUE)
  
  # Upload the file to S3
  put_object(file = file_path, object = file_name, bucket = s3BucketName)
}

loadData <- function() {
  # Get a list of all files
  file_names <- get_bucket_df(s3BucketName)[["Key"]]
  # Read all files into a list
  data <- lapply(file_names, function(x) {
    object <- get_object(x, s3BucketName)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data  
}