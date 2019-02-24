#' @importFrom magrittr %<>%
prep_column <- function(col){
  col %<>%
    iconv(to = "UTF-8") %>%
    tolower() %>%
    stringr::str_squish()
  return(col)
}


find_keywords <- function(col, json.input){

  indexer <- json.input %>%
    dplyr::mutate(match_locs = purrr::pmap(
      .,
      function(keywords, approx.match, false.positive.matches,...){
        out <- purrr::pmap(
          list(keywords, approx.match, false.positive.matches),
          ~ agrep(..1, col, max.distance = ..2, ignore.case = TRUE))
        return(out)
      }))

  return(indexer)
}

remove_false_positives <- function(col, sep, indexer){
  out <- col %>% rlang::as_list()
  if (!is.na(sep)) out %<>% stringr::str_split(sep)

  return(out)
}
