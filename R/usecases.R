

#' This is required to be ported to another package
#'
#' @export

enable_usecases <- function(){
  source(system.file("embedded","usecases.R",package = "patch"),
         echo = FALSE, local = FALSE)
  invisible(0)
}
