#' A Data Transformation Function
#'
#' This function will trim a variable(s) based on a zscore cutoff
#' @param x dataframe
#' @param variables variables c() to be trimmed
#' @param cutoff zscore cutoff to use for trimming (default: 3.5)
#' @param context Name of column to group by
#' @param replace What value should the trimmed values be replaced with. (default: replace = "NA")
#' @keywords trim
#' @export
#' @examples
#' trim(x, variables = c(), cutoff = 3.5)

trim <- function(x, variables = c(), cutoff = 3.5, context = "", replace = "NA"){
  x <- center(x, variables = variables, standardized = TRUE, context = context)

  if (replace=="NA") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x, placeholder = ifelse(get(zscored) > cutoff, NA,
                                                 ifelse(get(zscored) < (cutoff*-1), NA, get(i))))
      x <- dplyr::select(x, -(zscored), -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }

  if (replace=="cutoff") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      if (context!=""){
        if (length(context)==1){
          x <- dplyr::group_by(x, get(context))
          zscored <- paste(i, "_zwc", sep = "")
        } else if (length(context)==2){
          x <- dplyr::group_by(x, get(context[1]), get(context[2]))
          zscored <- paste(i, "_zwc", sep = "")
        } else if (length(context)==3){
          x <- dplyr::group_by(x, get(context[1]), get(context[2]), get(context[3]))
          zscored <- paste(i, "_zwc", sep = "")
        }
      }
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.mean = mean(get(i), na.rm = TRUE),
                         placeholder.sd = sd(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.mean + (placeholder.sd*cutoff),
                                              ifelse(placeholder < (cutoff*-1), placeholder.mean - (placeholder.sd*cutoff), get(i))))
      x <- dplyr::select(x, -(zscored), -(i), -placeholder.mean, -placeholder.sd)
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  return(x)
}
