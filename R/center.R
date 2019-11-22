#' Create a column of centered variables
#'
#' This function centers a variable around the mean. To create z-scores,
#' specify `standardize=TRUE`
#' A new column will be created with the centered values.
#' There is also the option to center within context.
#' @param x dataframe
#' @param variables c() of columns to standardize
#' @param standardize Logical. Do you want to calculate
#' zscores? (Default = FALSE)
#' @param drop Drop original non-centered variables
#' @export
#'

center <- function(x, variables = c(colnames(x)), standardize = FALSE,
                   drop = FALSE){
  # Perform this function for each variable specified
  for (variable in colnames(x[variables])){
    # Calculate centered scores using the scale() function
    x <- dplyr::mutate(x,
                       hold = scale(get(variable), center = TRUE,
                                    scale = standardize))
    x$hold <- as.vector(x$hold)
    if (standardize==FALSE){
      names(x)[which(names(x)=="hold")] <- paste(variable, "_c", sep = "")
    } else if (standardize==TRUE){
      names(x)[which(names(x)=="hold")] <- paste(variable, "_z", sep = "")
    }
  }

  if (drop == TRUE) {
    x <- x[, -which(colnames(x) %in% variables)]
  }

  return(x)
}

