#' Data Transformation Function
#'
#' This function is an extension of the spread() function in tidyverse.
#' It can multiple values to apply spread() on.
#' @param x dataframe
#' @param key The variable used for spreading
#' @param key_combine.name If using more than one key column then specify name of a new combined column
#' @param key_combine.sep if using more than one key column then specifiy how values should be seperated when combined
#' @param values A vector of columns that contain the values
#' @param by What column is not being reorganized and needs to be preserved. Usually "Subject"
#' @param fill Passed to spread() fill parameter
#' @keywords reshape
#' @export
#' @examples
#' reshape(x, key = "variable", values = c("value1", "value2"), by = "Subject")

reshape <- function(x, key = "", key_combine.name = "", key_combine.sep = "", values = c(), by = "", fill = NA){
  if (length(key)>1){
    x <- tidyr::unite(x, placeholder, key, sep = key_combine.sep)
    colnames(x)[which(colnames(x)=="placeholder")] <- key_combine.name
    key <- key_combine.name
  }
  if (by==""){
    y <- data.frame(placeholder = 1)
    for (value in values){
      z <- dplyr::select(x, key, value)
      z <- tidyr::spread(z, key = key, value = value, fill = fill)
      colnames(z) <- paste(colnames(z), value, sep = "_")
      y <- cbind(y, z)
    }
    y <- dplyr::select(y, -placeholder)
  } else {
    y <- data.frame(by = numeric(0))
    colnames(y) <- by
    for (value in values){
      z <- dplyr::select(x, by, key, value)
      z <- tidyr::spread(z, key = key, value = value, fill = fill)
      colnames(z)[which(colnames(z)!=by)] <- paste(colnames(z)[which(colnames(z)!=by)],
                                                   value, sep = "_")
      y <- full_join(y, z, by = by)
    }
  }
  return(y)
}
