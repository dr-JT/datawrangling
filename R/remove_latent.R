#' Remove subjects with too much missing data on a given latent construct
#'
#' This function will remove subjects based on amount of missing data per latent construct
#' @param x dataframe
#' @param factor.list list of factors and tasks. ex. `list(WMC = c("OSpan", "RotSpan", SymSpan"), Gf = c("RAPM", "NumberSeries", "LetterSets"))`
#' @param missing.allowed Proportion of tasks allowed to be missing
#' @param id Subject ID variable
#' @param output.dir File directory to save removed subjects to
#' @param output.file File name to save removed subjects to
#' @keywords remove
#' @export remove_latent
#' @examples
#' # Example

remove_latent <- function(x, factor.list, missing.allowed, id, output.dir = NULL, output.file = NULL){
  x.remove <- list()
  for (f in seq_along(factor.list)){
    x.remove[[f]] <- dplyr::mutate(x, missing = 0)
    for (task in factor.list[[f]]){
      x.remove[[f]] <- dplyr::mutate(x.remove[[f]], missing = ifelse(is.na(get(task)), missing + 1, missing))
    }
    x.remove[[f]] <- dplyr::mutate(x.remove[[f]], missing = missing/length(factor.list[[f]]))
    x.remove[[f]] <- dplyr::filter(x.remove[[f]], missing > missing.allowed)
    x.remove[[f]] <- dplyr::select(x.remove[[f]], (id), missing)
    colnames(x.remove[[f]])[which(colnames(x.remove[[f]])=="missing")] <- paste(names(factor.list[f]), "missing", sep = ".")
  }
  x.remove <- plyr::join_all(x.remove, by = id, type = "full")

  if (is.null(output.dir)){
    subj.remove <- unique(x.remove$Subject)
    x <- dplyr::filter(x, !(Subject %in% subj.remove))
  } else {
    x <- remove_save(x, x.remove, output.dir = output.dir, output.file = output.file)
  }
  return(x)
}
