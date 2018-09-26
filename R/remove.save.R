#' A Data Transformation Function
#'
#' This function centers a variable around the mean. A new column will be created with the
#' centered values. There is also the option to center within context.
#' @param x dataframe
#' @param remove the dataframe that contains subjects to be removed
#' @param output folder directory path to save removed data
#' @keywords remove
#' @export
#' @examples
#' remove.save(data, remove = data_remove, save = "data/remove", taskname = "Flanker")

remove.save <- function(x, remove, output.dir = NULL, output.file = NULL) {
  if (nrow(remove)>0){
    dir.create(output.dir, showWarnings = FALSE)
    readr::write_delim(remove, path = paste(output.dir, output.file, sep = "/"), delim = "\t", na = "")
    subj.remove <- unique(remove$Subject)
    ## Remove them!
    x <- dplyr::filter(x, !(Subject %in% subj.remove))
  }
  return(x)
}

