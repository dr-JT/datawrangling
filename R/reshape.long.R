#' Data Transformation Function
#'
#' This function is an extension of the spread() function in tidyverse.
#' It can multiple values to apply spread() on.
#' @param x dataframe
#' @param variables The variable used for spreading
#' @param variables_combine.name If using more than one variables column then specify name of a new combined column
#' @param variables_combine.sep if using more than one variables column then specifiy how values should be seperated when combined
#' @param values A vector of columns that contain the values
#' @param id What column is not being reorganized and needs to be preserved. Usually "Subject"
#' @param fill Passed to spread() fill parameter
#' @variableswords reshape
#' @export reshape.long
#' @examples
#' reshape.long(x, variables = "variable", values = c("value1", "value2"), by = "Subject")

reshape.long <- function(x, variables = "", variables_combine.sep = "_", values = c(), id = "", fill = NA){
  if (length(variables)>1){
    x <- tidyr::unite(x, placeholder, variables, sep = variables_combine.sep)
    variables <- "placeholder"
  }
  if (id==""){
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]], key = variables, value = values[i], fill = fill)
      colnames(y[[i]]) <- paste(colnames(y[[i]]), values[i], sep = ".")
    }
    x <- plyr::join_all(y)
  } else {
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, id, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]], key = variables, value = values[i], fill = fill)
      colnames(y[[i]])[which(colnames(y[[i]])!=id)] <- paste(colnames(y[[i]])[which(colnames(y[[i]])!=id)],
                                                   values[i], sep = ".")
    }
    x <- plyr::join_all(y, by = id)
  }
  return(x)
}
