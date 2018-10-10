#' A Tidy Data Function
#'
#' This function removes duplicate subject files from an E-Merge txt file
#' @param x Imported E-Merge txt file
#' @param timing.variables Variable name in file that corresponds to the session date and time
#' @param subj.variable Name of varaible with Subject information
#' @param taskname name of task for saving output file
#' @param output.folder Folder path to output the duplicate file to
#' @export
#' @examples
#' duplicates.remove(x, subset = c(colnames(x)), context = "Subject")

duplicates.remove <- function(x, timing.variables = "SessionStartDateTimeUtc", subj.variable = "Subject", taskname = "", output.folder = ""){
  x_duplicated <- x[,c(subj.variable, timing.variables)]
  x_duplicated <- x_duplicated[!duplicated(x_duplicated),]

  duplicated.subj <- x_duplicated[duplicated(x_duplicated[subj.variable]),][[subj.variable]]
  x_duplicated <- x_duplicated[which(x_duplicated[[subj.variable]] %in% duplicated.subj),]

  if (output.folder!=""){
    if (nrow(x_duplicated)>0){
      dir.create(output.folder, showWarnings = FALSE)
      readr::write_delim(x_duplicated, path = paste(output.folder, "/", taskname, "_duplicates.txt", sep = ""), delim = "\t", na = "")
    }
  }

  x <- x[which(!(x[[subj.variable]] %in% duplicated.subj)),]

  return(x)
}
