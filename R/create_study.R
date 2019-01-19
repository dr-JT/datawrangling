#' Create Directory Organization For a New Study
#'
#' This function can be used to automatically setup your study directory by
#' creating folders and a template masterscript
#' @param scripts.dir Logical. Create script directory? default = TRUE
#' @param data.dir Logical. Create data files directory? default = TRUE
#' @param raw.dir Logical. Create raw? default = TRUE
#' @param messy.dir Logical. Create raw messy directory? default = TRUE
#' @param scored.dir Logical. Create scored directory? default = TRUE
#' @param results.dir Logical. Create results directory? default = TRUE
#' @param figures.dir Logical. Create figures directory? default = TRUE
#' @param manuscript.dir Logical. Create manuscript directory? default = TRUE
#' @param presentations.dir Logical. Create presentations directory? default = TRUE
#' @param documents.dir Logical. Create documents directory? default = TRUE
#' @param other.dir List of other directories you want to create
#' @param masterscript Logical. Download masterscript template? default = TRUE
#' @keywords center
#' @export
#' @examples
#'

create_study <- function(scripts.dir = TRUE, data.dir = TRUE, raw.dir = TRUE,
                         messy.dir = TRUE, scored.dir = TRUE, results.dir = TRUE,
                         figures.dir = TRUE, manuscript.dir = TRUE,
                         presentations.dir = TRUE, documents.dir = TRUE,
                         other.dir = c(), masterscript = TRUE){

  ## Create directory structure
  if (scripts.dir==TRUE & dir.exists("R Scripts")==FALSE) dir.create("R Scripts")
  if (data.dir==TRUE & dir.exists("Data Files")==FALSE) dir.create("Data Files")
  if (raw.dir==TRUE & dir.exists("Data Files/Raw Data")==FALSE) dir.create("Data Files/Raw Data")
  if (messy.dir==TRUE & dir.exists(paste("Data Files/Raw Data/", messy.dir, sep = ""))==FALSE) dir.create(paste("Data Files/Raw Data/", messy.dir, sep = ""))
  if (scored.dir==TRUE & dir.exists("Data Files/Scored Data")==FALSE) dir.create("Data Files/Scored Data")
  if (results.dir==TRUE & dir.exists("Results")==FALSE) dir.create("Results")
  if (figures.dir==TRUE & dir.exists("Results/Figures")==FALSE) dir.create("Results/Figures")
  if (manuscript.dir==TRUE & dir.exists("Manuscript")==FALSE) dir.create("Manuscript")
  if (presentations.dir==TRUE & dir.exists("Presentations")==FALSE) dir.create("Presentations")
  if (documents.dir==TRUE & dir.exists("Documents")==FALSE) dir.create("Documents")
  for (dir in other.dir){
    if (dir.exists(dir)==FALSE) dir.create(dir)
  }

  ## Download Templates
  if(masterscript==TRUE) download.file("http://englelab.gatech.edu/R/masterscript.R", "")

}

