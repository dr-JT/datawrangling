#' Create a column of centered variables
#'
#' This function centers a variable around the mean. To create z-scores, specify `standardized=TRUE`
#' A new column will be created with the centered values.
#' There is also the option to center within context.
#' @param x dataframe
#' @param variables c() of columns to standardize
#' @param standardized Logical. Do you want to calculate zscores? (Default = FALSE)
#' @keywords center
#' @export
#' @examples
#' center(x, subset = c(colnames(x)), standardized = FALSE)

center <- function(x, variables = c(colnames(x)), standardized = FALSE){
  # Perform this function for each variable specified
  for (variable in colnames(x[variables])){
    # Calculate centered scores using the scale() function
    x <- dplyr::mutate(x, hold = scale(get(variable), center = TRUE, scale = standardized))
    x$hold <- as.vector(x$hold)
    if (standardized==FALSE){
      names(x)[which(names(x)=="hold")] <- paste(variable, "_c", sep = "")
    } else if (standardized==TRUE){
      names(x)[which(names(x)=="hold")] <- paste(variable, "_z", sep = "")
    }
  }
  return(x)
}

