#' A Data Transformation Function
#'
#' This function centers a variable around the mean. To create z-scores, specify `standardized=TRUE`
#' A new column will be created with the centered values.
#' There is also the option to center within context.
#' @param x dataframe
#' @param variables c() of columns to standardize
#' @param context Name of column to group by
#' @param standardized Logical. Do you want to calculate zscores? (Default = FALSE)
#' @keywords center
#' @export
#' @examples
#' center(x, subset = c(colnames(x)), context = "group", standardized = FALSE)

center <- function(x, variables = c(colnames(x)), context = "", standardized = FALSE){
  # Perform this function for each variable specified
  for (variable in colnames(x[variables])){
    # If context is specified, then group_by context. Can specify up to three levels of context
    if (context!=""){
      if (length(context)==1){
        x <- dplyr::group_by(x, get(context))
      } else if (length(context)==2){
        x <- dplyr::group_by(x, get(context[1]), get(context[2]))
      } else if (length(context)==3){
        x <- dplyr::group_by(x, get(context[1]), get(context[2]), get(context[3]))
      }
    }

    if (standardized==FALSE){
      # Calculate centered scores using the scale() function
      x <- dplyr::mutate(x, c.scored = scale(get(variable), center = TRUE, scale = FALSE))
      x$c.scored <- as.vector(x$c.scored)
      # If context is specified, then label new centered variables with _cwc
      # If context is not specified, then label new centered variables with _c
      if (context!=""){
        names(x)[which(names(x)=="c.scored")] <- paste(variable, "_cwc", sep = "")
      } else if (context==""){
        names(x)[which(names(x)=="c.scored")] <- paste(variable, "_c", sep = "")
      }
    } else if (standardized==TRUE){
      # Calculate zscores using the scale() function
      x <- dplyr::mutate(x, z.scored = scale(get(variable), center = TRUE, scale = TRUE))
      x$z.scored <- as.vector(x$z.scored)
      # If context is specified, then label new zscored variables with _zwc
      # If context is not specified, then label new zscored variables with _z
      if (context[1]!=""){
        names(x)[which(names(x)=="z.scored")] <- paste(variable, "_zwc", sep = "")
      } else if (context==""){
        names(x)[which(names(x)=="z.scored")] <- paste(variable, "_z", sep = "")
      }
    }

    # Ungroup dataframe
    x <- dplyr::ungroup(x)
  }
  # Remove unwanted columns
  x <- x[,!(names(x) %in% c("get(context)", "get(context[1])", "get(context[2])", "get(context[3])"))]
  return(x)
}

