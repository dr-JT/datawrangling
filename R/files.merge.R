#' A Data Transformation Function
#'
#' This function merges files of the same data structure
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param delim Delimiter used in files to be merged
#' @param na How are missing values defined in files to be merged
#' @export
#' @examples
#' files.merge(path = "./Data", pattern = ".txt")

files.merge <- function(path = "", pattern = "", delim = "\t", na = "NA", output.folder = ""){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  merged <- list()
  for (i in 1:length(filelist)){
    merged[[i]] <- readr::read_delim(filelist[[i]], delim, escape_double = FALSE, trim_ws = TRUE, na = na)
  }
  merged <- dplyr::bind_rows(merged)

  if (output.folder!=""){
    readr::write_delim(merged, path = output.folder, delim, na = na)
  }

  return(merged)
}
