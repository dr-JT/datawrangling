#' A Data Transformation Function
#'
#' This function centers a variable around the mean. A new column will be created with the
#' centered values. There is also the option to center within context.
#' @param x dataframe
#' @param remove the dataframe that contains subjects to be removed
#' @param save folder directory path to save removed data
#' @param taskname used to name the file
#' @keywords remove
#' @export
#' @examples
#' remove.save(data, remove = data_remove, save = "data/remove", taskname = "Flanker")

remove.save <- function(x, remove = "", save = "", taskname = "") {
  if (nrow(remove)>0){
    dir.create(save, showWarnings = FALSE)
    write_delim(remove, path = paste(save, "/", taskname, "_remove.txt", sep = ""), delim = "\t", na = "")
    subj.remove <- unique(remove$Subject)
    ## Remove them!
    x <- filter(x, !(Subject %in% subj.remove))
  }
  return(x)
}

