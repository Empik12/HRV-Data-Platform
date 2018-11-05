
generate_feedback_matrix <- function(Ncols,Nrows,DFRows){

    m = matrix(
      as.character(1:5), nrow =  as.numeric(Nrows), ncol = as.numeric(Ncols), byrow = TRUE,
      dimnames = list(DFRows, 1:5)
    )
    for (i in seq_len(nrow(m))) {
      m[i, ] = sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        DFRows[i], m[i, ]
      )
    }
    return(m)
}

merge_data_frames <- function(featuresDF, sequence){
  
  
  feedbackDF <- as.data.frame(generate_feedback_matrix(5,length(featuresDF$time),sequence))
  featuresFeadbackDF <- merge(featuresDF,feedbackDF, by=0, all=TRUE)
  
  featuresFeadbackDF <- featuresFeadbackDF[
    with(featuresFeadbackDF, order(as.numeric(featuresFeadbackDF$Row.names))),
    ]
  row.names(featuresFeadbackDF) <- featuresFeadbackDF$Row.names
  return(featuresFeadbackDF)
}



