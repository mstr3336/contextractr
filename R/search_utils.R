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
    dplyr::mutate(match_locs = purrr::map2(
      keywords, approx.match,
      function(keywords, approx.match){
        out <- purrr::map2(keywords, approx.match,
          ~ agrep(.x, col, max.distance = .y, ignore.case = TRUE))
        return(out)
      }))

  return(indexer)
}



index_kws_in_strings <- function(col, sep, indexer){
  split_col <- col %>% rlang::as_list()
  if (!is.na(sep)) split_col %<>% stringr::str_split(sep)

  out <- indexer %>%
    dplyr::mutate(kw_pos = purrr::pmap(
      .,
      function(keywords, approx.match, match_locs){
        # iterate through these columns
        out <- purrr::pmap(
          list(keywords, approx.match, match_locs),
          function(keyword, approx.match, match_locs, ...){
            # within each row, there are vectors of
            # keywords[n], approx.match[n],
            # & match_locs[n], which will nest a variable vector of integers
            # split_col should be tokenized s.t it is first split into spans,
            # then into words (so split col is list->list->chr
            cvecs <- split_col[match_locs]

            positions <- cvecs %>%
              purrr::map(~ agrep(keyword, ., max.distance = approx.match)) %>%
              purrr::set_names(nm = as.character(match_locs))
            return(positions)
          }
        )
      }))

  return(out)
}
