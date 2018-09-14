#' A Data Transformation Function
#'
#' This function merges files of the same data structure
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param delim Delimiter used in files to be merged
#' @param na How are missing values defined in files to be merged
#' @param output.file File name and path to be saved
#' @export
#' @examples
#' files.merge(path = "./Data", pattern = ".txt")

files.merge <- function(path = "", pattern = "", delim = "\t", na = "", output.file = ""){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  merged <- list()
  for (i in seq_along(filelist)){
    merged[[i]] <- readr::read_delim(filelist[[i]], delim, escape_double = FALSE, trim_ws = TRUE, na = na)
  }
  merged <- dplyr::bind_rows(merged)

  if (output.file!=""){
    readr::write_delim(merged, path = output.file, delim, na = na)
  }

  return(merged)
}
