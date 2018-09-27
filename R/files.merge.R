#' A Data Transformation Function
#'
#' This function merges files of the same data structure
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param delim Delimiter used in files to be merged
#' @param na How are missing values defined in files to be merged
#' @param output.file File name and path to be saved
#' @param id Subject ID variable name
#' @export
#' @examples
#' files.merge(path = "./Data", pattern = ".txt")

files.merge <- function(path = "", pattern = "", delim = "\t", na = "", output.file = "", id = NULL){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)){
    import[[i]] <- readr::read_delim(filelist[[i]], delim, escape_double = FALSE, trim_ws = TRUE, na = na)
  }
  merged <- plyr::join_all(import, by = id, type = "full")
  merged <- merged[, !duplicated(colnames(merged))]

  if (output.file!=""){
    readr::write_delim(merged, path = output.file, delim, na = na)
  }

  return(merged)
}
