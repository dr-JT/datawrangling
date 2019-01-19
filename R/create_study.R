#' Create Directory Organization For a New Study
#'
#' This function can be used to automatically setup your study directory by
#' creating folders and a template masterscript
#' @param scripts.dir Logical. Create script directory? default = TRUE
#' @keywords center
#' @export
#' @examples
#'

create_study <- function(scripts.dir = TRUE){

  ## Create directory structure
  if (scripts.dir==TRUE & dir.exists("R Scripts")==FALSE) dir.create("R Scripts")
  if (data.dir==TRUE & dir.existst("Data Files")==FALSE) dir.create("Data Files")
  if (raw.dir==TRUE & dir.exists("Data Files/Raw Data")==FALSE) dir.create("Data Files/Raw Data")
  if (messy.dir==TRUE & dir.exists(paste("Data Files/Raw Data/", mess.dir, sep = ""))==FALSE) dir.create(paste("Data Files/Raw Data/", mess.dir, sep = ""))
  if (scored.dir==TRUE & dir.exists("Data Files/Scored Data")==FALSE) dir.create("Data Files/Scored Data")
  if (results.dir==TRUE & dir.exists("Results")==FALSE) dir.create("Results")
  if (figures.dir==TRUE & dir.exists("Results/Figures")==FALSE) dir.create("Results/Figures")
  if (manuscript.dir==TRUE & dir.exists("Manuscript")==FALSE) dir.create("Manuscript")
  if (presentations.dir==TRUE & dir.exists("Presentations")==FALSE) dir.create("Presentations")
  if (documents.dir==TRUE & dir.exists("Documents")==FALSE) dir.create("Documents")
  for (dir in other.dirs){
    if (dir.exists(dir)==FALSE) dir.create(dir)
  }

  ## Download Templates
  download.file("http://englelab.gatech.edu/R/masterscript.R", "")

}

