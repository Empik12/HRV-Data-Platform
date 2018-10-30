
generate_feedback_matrix <- function(Ncols,Nrows){
    rowsInData <- seq(1,as.numeric(Nrows), by =1)
    m = matrix(
      as.character(1:5), nrow = as.numeric(Nrows), ncol = as.numeric(Ncols), byrow = TRUE,
      dimnames = list(rowsInData, 1:5)
    )
    for (i in seq_len(nrow(m))) {
      m[i, ] = sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        rowsInData[i], m[i, ]
      )
    }
    return(m)
}