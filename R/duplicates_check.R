#' Check and remove duplicate ids
#'
#' This function checks and removes duplicate ids
#' @param x dataframe
#' @param id Subject ID variable name.
#' @param unique Column names that are unique and should be used to
#'     check for duplicate id's
#' @param n Number of unique id's expected
#' @param remove logical. Remove duplicate ids from data?
#' @param save_as Folder path and file name to output the duplicate ID's
#'

duplicates_check <- function(x, id = "Subject",
                             unique = c("SessionDate", "SessionTime"),
                             save_as = NULL){
  # get duplicate ids
  duplicates <- dplyr::select(x, id, unique)
  duplicates <- dplyr::distinct(duplicates)
  duplicates <- dplyr::group_by(duplicates, get(id))
  duplicates <- dplyr::mutate(duplicates, count = n())
  duplicates <- dplyr::ungroup()
  duplicates <- dplyr::filter(duplicates, count > n)
  duplicates <- dplyr::select(duplicates, id, unique)

  # save duplicates to file
  if (!is.null(save_as)) {
    if (nrow(duplicates) > 0) {
      folder <- dirname(save_as)
      if (dir.exists(folder) == FALSE) {
        dir.create(folder, showWarnings = FALSE)
      }
      readr::write_csv(duplicates, save_as)
    }
  }

  # remove duplicates
  if (nrow(duplicates) > 0) {
    ids_duplicates <- duplicates[[id]]
    if (remove == TRUE) {
      x <- dplyr::filter(x, !(get(id) %in% ids_duplicates))
      message(paste("Duplicate IDs found AND removed:",
                    ids_duplicates, sep = " "))
    } else {
      message(paste("Duplicate IDs found BUT not removed:",
                    ids_duplicates, sep = " "))
    }
  } else {
    message("No duplicate IDs found!")
  }
  return(x)
}
