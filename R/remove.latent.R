#' A Data Transformation Function
#'
#' This function will remove subjects based on amount of missing data per latent construct
#' @param x dataframe
#' @param factor.list list of factors and tasks
#' @param missing.allowed % of tasks allowed to be missing
#' @keywords remove
#' @export remove.latent
#' @examples
#' remove.latent(x, variables = c(), cutoff = 3.5)

remove.latent <- function(x, factor.list, missing.allowed){
  for (factor in factor.list){
    x <- dplyr::mutate(x, missing = 0)
    for (task in factor.list[[factor]]){
      x <- dplyr::mutate(x, missing = ifelse(is.na(get(task)), missing + 1, missing))
    }
    x <- dplyr::mutate(x, missing = missing/length(factor.list[[factor]]))
    x <- dplyr::filter(x, missing > missing.allowed)
  }
  x <- dplyr::mutate(x)
}
