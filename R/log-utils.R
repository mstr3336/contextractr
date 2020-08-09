
#' Log an object in its native print representation (with newlines)
#'
#' @keywords internal
#' @param x the object to be displayed
pretty_string <- function(x, ...){
  out <- utils::capture.output(print(x, ...)) %>%
    glue::glue_collapse(sep = "\n")

  return(out)
}

L <- logging::getLogger("contextractr")
