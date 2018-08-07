#' A Data Transformation Function
#'
#' This function will trim a variable(s) based on a zscore cutoff
#' @param x dataframe
#' @param variables variables c() to be trimmed
#' @param cutoff zscore cutoff to use for trimming (default: 3.5)
#' @keywords trim
#' @export
#' @examples
#' trim(x, variables = c(), cutoff = 3.5)

trim <- function(x, variables = c(), cutoff = 3.5){
  x <- center(x, variables = variables, standardized = TRUE)
  for (i in variables){
    zscored <- paste(i, "_z", sep = "")
    x <- dplyr::mutate(x, filter = ifelse(get(zscored)>cutoff,NA,ifelse(get(zscored)<(cutoff*-1),NA,get(i))))
    x[i] <- x["filter"]
    x <- dplyr::select(x, -(zscored))
  }
  x <- dplyr::select(x, -filter)
}
