#' Data Transformation Function
#'
#' This function is an extension of the spread() function in tidyverse.
#' It can multiple values to apply spread() on.
#' @param x dataframe
#' @param variable.name The variable used for spreading
#' @param values If using more than one variables column then specify name of a new combined column
#' @param id What column is not being reorganized and needs to be preserved. Usually "Subject"
#' @param separate.into Passed to spread() fill parameter
#' @param separate.pattern stuff
#' @keywords reshape
#' @export reshape.gather
#' @examples
#' reshape.gather(x, variables = "variable", values = c("value1", "value2"), by = "Subject")

reshape.gather <- function(x, variable.name, values, id = NULL, separate.into = NULL, separate.pattern = NULL){
  if (!is.null(separate.into)){
    variable.name <- "placeholder"
  }

  if (is.null(id)){
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, contains(values[i]))
      columns <- colnames(y[[i]])[which(colnames(y[[i]])!=id)]
      y <- tidyr::gather(y[[i]], key = variable.name, value = values[i], columns)
    }
    x <- plyr::join_all(y, by = variable.name)
  } else {
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, id, contains(values[i]))
      columns <- colnames(y[[i]])[which(colnames(y[[i]])!=id)]
      y <- tidyr::gather(y[[i]], key = variable.name, value = values[i], columns)
    }
    x <- plyr::join_all(y, by = c(id, variable.name))
  }

  if (!is.null(separate.into)){
    x <- tidyr::separate(x, variable.name, into = separate.into, sep = separate.pattern)
  }
  return(x)
}
