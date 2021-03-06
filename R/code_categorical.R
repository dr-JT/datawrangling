#' Recode a character variable into numerical values
#'
#' This function will code a column containing categorical "string" values into numerical values
#' @param x dataframe
#' @param variable column name
#' @param order c() of column values in order
#' @keywords code
#' @export
#' @examples
#' code_categorical(x, variable = "columnName", order = c("string1", "string2", "string3"))

code_categorical <- function(x, variable = "", order = c()){
  for (value in 1:length(order)){
    x[which(x[variable]==order[value]),variable] <- value
  }
  x[variable] <- as.numeric(x[[variable]])
  return(x)
}
