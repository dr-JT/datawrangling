#' Reshape dataframe from long to wide
#'
#' This function is an extension of the `tidyr::spread()` function
#' It can `tidyr::spread()` on multiple keys and values.
#' @param x dataframe
#' @param variables The variable used for spreading
#' @param variables_combine.name If using more than one variables column then specify name of a new combined column
#' @param variables_combine.sep if using more than one variables column then specifiy how values should be seperated when combined
#' @param values A vector of columns that contain the values
#' @param id What column is not being reorganized and needs to be preserved. Usually "Subject"
#' @param fill Passed to spread() fill parameter
#' @keywords reshape
#' @export reshape_spread
#' @examples
#' reshape_spread(x, variables = "variable", values = c("value1", "value2"), by = "Subject")

reshape_spread <- function(x, variables = "", variables_combine.sep = "_", values = c(), id = "", fill = NA){
  if (length(variables)>1){
    x <- tidyr::unite(x, placeholder, variables, sep = variables_combine.sep)
    variables <- "placeholder"
  }
  if (id==""){
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]], key = variables, value = values[i], fill = fill)
      colnames(y[[i]]) <- paste(colnames(y[[i]]), values[i], sep = "_")
    }
    x <- plyr::join_all(y, type="full")
  } else {
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, id, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]], key = variables, value = values[i], fill = fill)
      colnames(y[[i]])[which(colnames(y[[i]])!=id)] <- paste(colnames(y[[i]])[which(colnames(y[[i]])!=id)],
                                                   values[i], sep = "_")
    }
    x <- plyr::join_all(y, by = id, type = "full")
  }
  return(x)
}
