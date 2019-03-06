#' Convert a CSV into a serial mapping suitable for Contextractr
#'
#' Given a csv, with one row per keyword + approx.match specifier, grouped
#'     according to "title" (Single value for title may span multiple keywords)
#'     transform this to something digestable by contextractr.
#' @param df the input dataframe, more or less a raw csv
#' @return the input df, serialized.
#' @export
#' @concept IO
#' @examples
#' if (requireNamespace("readr", quietly = TRUE)){
#'   in_csv <- readr::read_csv("../../tests/testthat/csv_specifier.csv")
#'   print(in_csv)
#'
#'   out <- in_csv %>% csv_to_serial()
#'   if (requireNamespace("utils", quietly = TRUE)) utils::str(out)
#' }
csv_to_serial <- function(df){
  title        <- quo(title)
  keywords     <- quo(keywords)
  approx.match <- quo(approx.match)

  df %<>% dplyr::select(-dplyr::matches("comments"))

  df %<>%
    tidyr::replace_na(list(approx.match = 0.0)) %>%
    dplyr::group_by(!!title) %>%
    tidyr::nest(!!keywords, !!approx.match)
  df %<>%
    dplyr::mutate(!! "keywords"     := purrr::map(data, "keywords"),
                  !! "approx.match" := purrr::map(data, "approx.match")) %>%
    dplyr::select(-data)
  out <- df %>%
    purrr::transpose()
  return(out)
}
