#' generate example Rmd files
#'
#' @param exampleName A character.
#'
#' @return
#' @export
#'
#' @examples none
emajor_example <- function(exampleName){
  # exampleName="emajor"
  writeLines(
    exampleName,
    file.path(getwd(),paste0(exampleName,".Rmd"))
  )

}
