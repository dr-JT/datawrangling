#' A Data Transformation Function
#'
#' This function will remove subjects based on amount of missing data per latent construct
#' @param x dataframe
#' @param factor.list list of factors and tasks
#' @param missing.allowed % of tasks allowed to be missing
#' @param id Subject ID variable
#' @param output.removed file path and name to save removed subjects to
#' @keywords remove
#' @export remove.latent
#' @examples
#' remove.latent(x, variables = c(), cutoff = 3.5)

remove.latent <- function(x, factor.list, missing.allowed, id, output.removed = NULL){
  x.remove <- x
  for (f in seq_along(factor.list)){
    x.remove <- dplyr::mutate(x.remove, missing = 0)
    for (task in factor.list[[f]]){
      x.remove <- dplyr::mutate(x.remove, missing = ifelse(is.na(get(task)), missing + 1, missing))
    }
    x.remove <- dplyr::mutate(x.remove, missing = missing/length(factor.list[[f]]))
    x.remove <- dplyr::filter(x.remove, missing > missing.allowed)
    colnames(x.remove)[which(colnames(x.remove)=="missing")] <- paste(names(factor.list[f]), "missing", sep = ".")
  }
  x <- dplyr::select(x, id, dplyr::contains("missing"))

  if (is.null(output.removed)){
    subj.remove <- unique(x.remove$Subject)
    x <- dplyr::filter(x, !(Subject %in% subj.remove))
  } else {
    x <- remove.save(x, x.remove, output = output.removed)
  }
  return(x)
}
