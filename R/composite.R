#' A Data Transformation Function
#'
#' This function will create a composite variable
#' @param x dataframe
#' @param variables c() of columns to average together to create the composite
#' @param comp How the composite should be calculated, i.e. mean or sum. (Default = "mean").
#' @param standardize Logical. Do you want to calculate the composite based on standardized (z-score) values? (Default = FALSE)
#' @param name Name of the new composite variable
#' @param missing.criteria Criteria for how many variables can having missing values to not add to composite variable
#' @keywords composite
#' @export
#' @examples
#' composite(x, variable = c(), comp = "mean", name = "name")

composite <- function (x, variables = c(), comp = "mean", standardized = FALSE, name = "", missing.criteria = ""){
  # Compute z-scores if standardized==TRUE
  if (standardized==TRUE){
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
  if (comp=="mean"){
    x <- transform(x, composite = rowMeans(x[variables], na.rm = TRUE))
  } else if (comp=="sum"){
    x <- transform(x, composite = rowSums(x[variables], na.rm = TRUE))
  }

  # If missing criteria is specified, then index for each subject how many of the variables they have missing values on
  # Then set composite to NA if they exceed the missing criteria
  if (missing.criteria!=""){
    x <- dplyr::mutate(x, Missing = 0)
    for (variable in colnames(x[variables])){
      x <- dplyr::mutate(x, Missing = ifelse(is.na(get(variable)),(Missing+1),(Missing+0)))
    }
    x <- dplyr::mutate(x, composite = ifelse(Missing>missing.criteria,NA,composite))
  }

  # Name composite variable and remove the Missing column
  colnames(x)[which(colnames(x)=="composite")] <- name
  remove <- c(variables, "Missing")
  x <- x[,-which(names(x) %in% remove)]

  return(x)
}