#' Remove outliers/bad subjects
#'
#' This function takes as import two dataframes. The first dataframe is the original dataframe containing all IDs,
#' The second dataframe contains only those IDs to be removed.
#' @param x dataframe
#' @param remove the dataframe that contains subjects to be removed
#' @param output folder directory path to save removed data
#' @param id Column name containing Subject IDs.
#' @keywords remove
#' @export
#' @examples
#' remove_save(data, remove = data_remove, save = "data/remove", taskname = "Flanker")

remove_save <- function(x, remove, output.dir = NULL, output.file = NULL, id = "Subject") {
  colnames(x)[which(colnames(x)==id)] <- "Subject"
  if (nrow(remove)>0){
    dir.create(output.dir, showWarnings = FALSE)
    readr::write_csv(remove, paste(output.dir, output.file, sep = "/"), na = "")
    colnames(remove)[which(colnames(remove)==id)] <- "Subject"
    subj.remove <- unique(remove$Subject)
    ## Remove them!
    x <- dplyr::filter(x, !(Subject %in% subj.remove))
  }
  colnames(x)[which(colnames(x)=="Subject")] <- id
  return(x)
}

