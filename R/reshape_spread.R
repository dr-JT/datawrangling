#' Reshape dataframe from long to wide
#'
#' This function is an extension of the `tidyr::spread()` function
#' It can `tidyr::spread()` on multiple keys and values.
#' @param x dataframe
#' @param variables The variable used for spreading
#'     (The variables/conditions that values are grouped within)
#' @param values A vector of columns that contain the values
#' @param id What column is not being reorganized and needs to be preserved.
#'     Usually "Subject"
#' @param colname.order The variable/value order to use when naming new columns.
#'     "variable_value" or "value_variable"
#' @param variables_combine.sep if using more than one variables column then
#'     specifiy how values should be seperated when combined
#' @param fill Passed to spread() fill parameter
#' @export reshape_spread
#' @examples
#'

reshape_spread <- function(x, variables = "", values = c(), id = "",
                           colname.order = "variable_value",
                           variables_combine.sep = "_", fill = NA){
  if (length(variables) > 1) {
    x <- tidyr::unite(x, placeholder, variables, sep = variables_combine.sep)
    variables <- "placeholder"
  }

  if (id == "") {
    y <- list()
    for (i in seq_along(values)) {
      y[[i]] <- dplyr::select(x, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]],
                              key = variables, value = values[i], fill = fill)
      if (colname.order == "variable_value") {
        colnames(y[[i]]) <- paste(colnames(y[[i]]), values[i], sep = "_")
      } else {
        colnames(y[[i]]) <- paste(values[i], colnames(y[[i]]), sep = "_")
      }

    }
    x <- plyr::join_all(y, type = "full")
  } else {
    y <- list()
    for (i in seq_along(values)) {
      y[[i]] <- dplyr::select(x, id, variables, values[i])
      y[[i]] <- tidyr::spread(y[[i]],
                              key = variables, value = values[i], fill = fill)

      if (colname.order == "variable_value") {
        colnames(y[[i]])[which(colnames(y[[i]])!=id)] <-
          paste(colnames(y[[i]])[which(colnames(y[[i]]) != id)],
                values[i],
                sep = "_")
      } else {
        colnames(y[[i]])[which(colnames(y[[i]])!=id)] <-
          paste(values[i],
                colnames(y[[i]])[which(colnames(y[[i]]) != id)],
                sep = "_")
      }


      y[[i]] <- as.data.frame(y[[i]])
    }
    x <- plyr::join_all(y, by = id, type = "full")
  }
  return(x)
}
