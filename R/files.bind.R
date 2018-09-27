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
#' files.bind(path = "./Data", pattern = ".txt")

files.bind <- function(path = "", pattern = "", delim = "\t", na = "", output.file = "", bind = "rows"){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)){
    import[[i]] <- readr::read_delim(filelist[[i]], delim, escape_double = FALSE, trim_ws = TRUE, na = na)
  }

  if (bind=="rows"){
    bound <- dplyr::bind_rows(import)
  }
  if (bind=="columns"){
    bound <- dplyr::bind_cols(import)
  }


  if (output.file!=""){
    readr::write_delim(bound, path = output.file, delim, na = na)
  }
  return(bound)
}
