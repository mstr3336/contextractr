

#' @importFrom glue glue
#' @importFrom rlang quo quos enquo enquos

add_keyword_cols <- function(.df, col,indexer){

  # By unnesting keywords, approxmatch, match_locs
  # and unnesting match_locs again, a unique index for each row is made
  unnest_cols <- rlang::quos(keywords, approx.match, match_locs)
  preserve <-  indexer %>% names() %>%
    setdiff(purrr::map_chr(unnest_cols,rlang::as_name))

  indexer %<>%
    tidyr::unnest(!!!unnest_cols, .preserve = dplyr::one_of(preserve)) %>%
    dplyr::mutate_at(dplyr::vars(!!!unnest_cols[1:2]), unlist) %>%
    tidyr::unnest(!!!unnest_cols[3], .preserve = dplyr::one_of(preserve))

  # We need idx so we can join it on match locs
  # A nested tibble col will be added for each corresponding match
  out <- .df %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::nest_join(indexer, by = c("idx" = "match_locs"), name = "indexer")

  return(out)

}
