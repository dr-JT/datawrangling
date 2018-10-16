#' Reshape dataframe from wide to long
#'
#' This function is an extension of the `tidyr::gather()` function.
#' It can `tidyr::gather()` on multiple keys and values
#' @param x dataframe
#' @param variable.names The variable used for gathering
#' @param values If using more than one variables column then specify name of a new combined column
#' @param id What column is not being reorganized and needs to be preserved. Usually "Subject"
#' @param separate.pattern stuff
#' @keywords reshape
#' @export reshape.gather
#' @examples
#' reshape.gather(x, variables = "variable", values = c("value1", "value2"), by = "Subject")

reshape.gather <- function(x, variable.names, values, id = NULL, separate.pattern = NULL){
  if (length(variable.names)>1){
    separate.into <- variable.names
    variable.names <- "variable.hold"
  } else {
    separate.into <- NULL
  }

  if (is.null(id)){
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, contains(values[i]))
      columns <- colnames(y[[i]])[which(colnames(y[[i]])!=id)]
      y[[i]] <- tidyr::gather(y[[i]], key = variable.hold, value = value.hold, columns)
      y[[i]]$variable.hold <- gsub(paste(".", values[i], sep = ""), "", y[[i]]$variable.hold)
      colnames(y[[i]])[which(colnames(y[[i]])=="variable.hold")] <- variable.names
      colnames(y[[i]])[which(colnames(y[[i]])=="value.hold")] <- values[i]
    }
    x <- plyr::join_all(y, by = variable.names, type="full")
  } else {
    y <- list()
    for (i in seq_along(values)){
      y[[i]] <- dplyr::select(x, id, contains(values[i]))
      columns <- colnames(y[[i]])[which(colnames(y[[i]])!=id)]
      y[[i]] <- tidyr::gather(y[[i]], key = variable.hold, value = value.hold, columns)
      y[[i]]$variable.hold <- gsub(paste(".", values[i], sep = ""), "", y[[i]]$variable.hold)
      colnames(y[[i]])[which(colnames(y[[i]])=="variable.hold")] <- variable.names
      colnames(y[[i]])[which(colnames(y[[i]])=="value.hold")] <- values[i]
    }
    x <- plyr::join_all(y, by = c(id, variable.names), type="full")
  }

  if (!is.null(separate.into)){
    x <- tidyr::separate(x, variable.names, into = separate.into, sep = separate.pattern)
  }

  if (!is.null(id)){
    x <- dplyr::arrange(x, get(id))
  }
  return(x)
}
