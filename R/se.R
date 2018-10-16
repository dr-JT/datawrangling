#' Calculate the standard error
#'
#' Calculates the standard error of the mean
#' @param x vector of values to calculate the se
#' @param na.rm Logical. Remove missing values in calculation?
#' @keywords se
#' @export
#' @examples
#' se(x)

se <- function(x, na.rm = FALSE){
  if (na.rm==TRUE){
    x <- x[which(!is.na(x))]
  }
  se <- sd(x)/sqrt(length(x))
  return(se)
}
