#' Bind multiple files into one file
#'
#' This function binds multiple files together. `bind` can occur by "rows" or "columns"
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param delim Delimiter used in files. Passed onto `readr::read_delim()`
#' @param output.file File name and path to be saved to.
#' @param guess_max Maximum
#' @export
#' @examples
#' files_bind(path = "./Data", pattern = ".txt")

files_bind <- function(path = "", pattern = "", delim = ",",
                       output.delim = ",", output.file = "",
                       bind = "rows", guess_max = 1000){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)){
    if (delim==","){
      import[[i]] <- readr::read_csv(filelist[[i]], guess_max = guess_max)
    }

    if (delim != ","){
      import[[i]] <- readr::read_delim(filelist[[i]],
                                       delim,
                                       escape_double = FALSE,
                                       trim_ws = TRUE,
                                       guess_max = guess_max)
    }
  }

  if (bind=="rows"){
    bound <- dplyr::bind_rows(import)
  }
  if (bind=="columns"|bind=="cols"){
    bound <- dplyr::bind_cols(import)
  }


  if (output.file!=""){
    if (output.delim==","){
      readr::write_csv(bound, output.file)
    }

    if (output.delim=="\t"){
      readr::write_delim(bound, path = output.file, delim)
    }
  }
  return(bound)
}
