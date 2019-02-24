
serial_as_tbl <- function(json_list){
  gotta_have <- c("title", "keywords", "approx.match", "false.positive.matches",
                  "prefix", "suffix", "prefix.approx.match",
                  "suffix.approx.match", "suffix.descriptions", "prefix.class",
                  "suffix.class", "ignore.prefix", "ignore.suffix",
                  "ignore.prefix.approx.match", "ignore.suffix.approx.match")

  nms <- json_list %>%  purrr::map(names) %>% purrr::reduce(union)
  out <- json_list %>%  purrr::transpose(.names = nms) %>% tibble::as_tibble()
  missing <- gotta_have %>% setdiff(nms)

  out %<>% dplyr::mutate_at(missing, ~ NA)
  return(out)
}
