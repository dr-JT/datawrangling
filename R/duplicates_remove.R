#' Remove duplicate subjects from an edat file
#'
#' This function removes duplicate subject files from an E-Merge txt file
#' @param x Imported E-Merge txt file
#' @param timing.variables Variable name in file that corresponds to the session date and time
#' @param subj.variable Name of varaible with Subject information
#' @param taskname name of task for saving output file
#' @param output.folder Folder path to output the duplicate file to
#' @export
#' @examples
#' duplicates_remove(x, subset = c(colnames(x)), context = "Subject")

duplicates_remove <- function(x, timing.variables = c("SessionDate", "SessionTime"),
                              subj.variable = "Subject", taskname = "",
                              output.folder = ""){
  x_duplicates <- x[,c(subj.variable, timing.variables)]
  x_duplicates <- dplyr::distinct(x_duplicates)
  subj_duplicate <- x_duplicates[which(duplicated(x_duplicates[, subj.variable])),]
  subj_duplicate <- subj_duplicate[[subj.variable]]

  x_duplicates <- x_duplicates[which(x_duplicates[[subj.variable]] %in% subj_duplicate), ]
  x_duplicates <- dplyr::arrange(x_duplicates, get(subj.variable))
  x <- x[which(!(x[[subj.variable]] %in% subj_duplicate)),]

  if (output.folder!=""){
    if (nrow(x_duplicates)>0){
      dir.create(output.folder, showWarnings = FALSE)
      readr::write_csv(x_duplicates, paste(output.folder, "/", taskname, "_duplicates.csv", sep = ""), na = "")
    }
  }
  return(x)
}
