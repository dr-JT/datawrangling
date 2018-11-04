#' Trim scores
#'
#' This function will trim variable(s) based on a zscore cutoff
#' @param x dataframe
#' @param variables variables c() to be trimmed
#' @param cutoff zscore cutoff to use for trimming (default: 3.5)
#' @param replace What value should the trimmed values be replaced with. (default: replace = "NA")
#' @keywords trim
#' @export
#' @examples
#' trim(x, variables = c(), cutoff = 3.5)

trim <- function(x, variables, cutoff = 3.5, replace = "NA", id = ""){
  col.order <- colnames(x)
  if (variables=="all"){
    variables <- colnames(x)[which(colnames(x)!=id)]
  } else {
    variables <- variables[which(!(variables!=id))]
  }

  x <- center(x, variables = variables, standardized = TRUE)

  if (replace=="NA") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = ifelse(get(zscored) > cutoff, NA,
                                                 ifelse(get(zscored) < (cutoff*-1), NA, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }

  if (replace=="cutoff") {
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.mean = mean(get(i), na.rm = TRUE),
                         placeholder.sd = sd(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.mean + (placeholder.sd*cutoff),
                                              ifelse(placeholder < (cutoff*-1), placeholder.mean - (placeholder.sd*cutoff), get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }

  if (replace=="mean"){
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.mean = mean(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.mean,
                                              ifelse(placeholder < (cutoff*-1), placeholder.mean, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }

  if (replace=="median"){
    for (i in variables){
      zscored <- paste(i, "_z", sep = "")
      x <- dplyr::mutate(x,
                         placeholder = get(zscored),
                         placeholder.median = median(get(i), na.rm = TRUE),
                         placeholder = ifelse(placeholder > cutoff, placeholder.median,
                                              ifelse(placeholder < (cutoff*-1), placeholder.median, get(i))))
      x <- dplyr::select(x, -(i))
      colnames(x)[which(colnames(x)=="placeholder")] <- i
    }
  }
  x <- x[col.order]
  return(x)
}
