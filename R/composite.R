#' Create a compositve variable
#'
#' This function will create a composite variable
#' @param x dataframe
#' @param variables c() of columns to average together to create the composite
#' @param type What type of composite should be calculated?, i.e. mean or sum. (Default = "mean").
#' @param standardize Logical. Do you want to calculate the composite based on standardize (z-score) values? (Default = FALSE)
#' @param name Name of the new composite variable
#' @param missing.allowed Criteria for how many variables can having missing values and still calculate a composite
#' @keywords composite
#' @export
#' @examples
#' composite(x, variable = c(), type = "mean", name = "name")

composite <- function (x, variables, type = "mean", standardize = TRUE, name = NULL, missing.allowed = NULL){
  # Compute z-scores if standardize==TRUE
  if (standardize==TRUE){
    for (variable in colnames(x[variables])){
      # Calculate zscores using the scale() function
      x <- dplyr::mutate(x, z.scored = scale(get(variable), center = TRUE, scale = TRUE))
      x$z.scored <- as.vector(x$z.scored)
      # Label new zscored variables with _z
      names(x)[which(names(x)=="z.scored")] <- paste(variable, "_z", sep = "")
    }
    # Add _z to variable list so that the composite will use the z-scored values
    variables <- paste(variables, "_z", sep = "")
  }

  # Compute composite based on comp parameter (mean or sum)
  if (type=="mean"){
    x <- transform(x, composite = rowMeans(x[variables], na.rm = TRUE))
  } else if (type=="sum"){
    x <- transform(x, composite = rowSums(x[variables], na.rm = TRUE))
  }

  # If missing criteria is specified, then index for each subject how many of the variables they have missing values on
  # Then set composite to NA if they exceed the missing criteria
  if (!is.null(missing.allowed)){
    x <- dplyr::mutate(x, Missing = 0)
    for (variable in colnames(x[variables])){
      x <- dplyr::mutate(x, Missing = ifelse(is.na(get(variable)),(Missing+1),(Missing+0)))
    }
    x <- dplyr::mutate(x, composite = ifelse(Missing>missing.allowed,NA,composite))
  }

  # Name composite variable and remove the Missing column
  colnames(x)[which(colnames(x)=="composite")] <- name
  remove <- c(variables, "Missing")
  x <- x[,-which(names(x) %in% remove)]

  return(x)
}
