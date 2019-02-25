

#' @export
Contextractr <- R6::R6Class(
  "Contextractr",
  public = list(

  ),
  private = list(
    mapping = NULL,
    map_cols = c(
      "title", "keywords", "approx.match", "false.positive.matches",
      "prefix", "suffix", "prefix.approx.match",
      "suffix.approx.match", "suffix.descriptions", "prefix.class",
      "suffix.class", "ignore.prefix", "ignore.suffix",
      "ignore.prefix.approx.match", "ignore.suffix.approx.match"),
    prefix_length = 4,
    suffix_length = 4,
    prefix_ignore = ".*\\.",
    suffix_ignore = "\\..*",
    span_sep = "[\\.\n]+",
    sep = "\\s+"
  )
  )

# ~~~~~ ========
# PUBLIC ====================================


# Initialize ======

#' Create a new instance of Contextractr
#'
#' Provide a mapping path, mapping list, or mapping tibble to allow setting of
#'     the mapping dictionary
#' @name Contextractr$new
#' @family Contextractr
#' @importFrom magrittr %<>%
NULL

Contextractr$set(
  "public", "initialize",
  function(json = NULL, yaml = NULL, serial = NULL, tibble = NULL){
    handlers <- list(json   = self$add_json,
                     yaml   = self$add_yaml,
                     serial = self$add_serial,
                     tibble = private$add_mapping_entries)
    inputs   <- list(json = json,
                     yaml = yaml,
                     serial = serial,
                     tibble = tibble)

    for (nm in names(inputs)){
      if (!rlang::is_empty(inputs[[nm]])) inputs[[nm]] %>% handlers[[nm]]()
    }
    return(invisible(self))
  }
)

# add_serial =====

#' Add a serial mapping
#' @name Contextractr$add_serial
#' @family Contextractr
#' @importFrom magrittr %<>%
NULL

Contextractr$set(
  "public", "add_serial",
  function(mapping){
    mapping %>% private$serial_as_tbl() %>% private$add_mapping_entries()
    return(invisible(self))
  }
)

# add_json =======

#' Add a json mapping
#' @name Contextractr$add_json
#' @family Contextractr
NULL

Contextractr$set(
  "public", "add_json",
  function(path){
    if (rlang::is_empty(path)) {
      print("Empty path given to add_json")
      return(invisible(self))
    }
    path %>% jsonlite::read_json() %>% self$add_serial()
    return(invisible(self))
  }
)

# add_yaml =====

#' Add a yaml mapping
#' @name Contextractr$add_yaml
#' @family Contextractr
NULL

Contextractr$set(
  "public", "add_yaml",
  function(path){
    if (rlang::is_empty(path)){
      print("Empty path given to add_yaml")
      return(invisible(self))
    }
    path %>% yaml::read_yaml() %>% self$add_serial()
    return(invisible(self))
  }
)

# locate_keywords =====
#' Locate/classify columns according to keywords
#' @name Contextractr$locate_keywords
#' @family Contextractr
#' @param .df the dataframe to process
#' @param selection the unquoted column for contextractr to look at
#' @return the input dataframe, with extra columns appended corresponding to
#'         the keywords found
#' @importFrom glue glue
NULL
Contextractr$set(
  "public", "locate_keywords",
  function(.df, selection){
    selection <- rlang::enquo(selection)
    col <- selection %>% rlang::as_name()

    idx <- .df %>%
      dplyr::pull(col) %>%
      private$find_keywords(private$mapping)

    out <- .df %>% private$add_keyword_cols(col, idx)
    return(out)
  }
)

# ~~~~~ ========================
# PRIVATE ======================================

# serial_to_tbl ====

Contextractr$set(
  "private", "serial_as_tbl",
  function(serial_list){
    nms <- serial_list %>%
      purrr::map(names) %>% purrr::reduce(union)

    out <- serial_list %>%
      purrr::transpose(.names = nms) %>% tibble::as_tibble()
    # Fill in the blanks for any missing cols
    missing <- private$map_cols %>% setdiff(nms)
    out %<>%
      dplyr::mutate_at(missing, ~ NA) %>%
      dplyr::mutate_at(c("title"), as.character)
    return(out)
  }
)

# add_mapping_entries ====

#' @importFrom magrittr %<>%

Contextractr$set(
  "private", "add_mapping_entries",
  function(mappings){
    # TODO: validation


    private$mapping %<>% dplyr::bind_rows(mappings)
    return(invisible(self))
  }
)

# prep_column =====

Contextractr$set(
  "private", "prep_column",
  function(col){
    col %<>%
      iconv(to = "UTF-8") %>%
      tolower() %>%
      stringr::str_squish()
    return(col)
  }
)

# find_keywords =====

Contextractr$set(
  "private", "find_keywords",
  function(col, mapping){

    indexer <- mapping %>%
      dplyr::mutate(match_locs = purrr::map2(
        keywords, approx.match,
        function(keywords, approx.match){
          out <- purrr::map2(keywords, approx.match,
                             ~ agrepl(.x, col, max.distance = .y, ignore.case = TRUE))
          return(out)
        }))

    return(indexer)
  }
)

# add_keyword_cols ====

#' @importFrom glue glue

Contextractr$set(
  "private", "add_keyword_cols",
  function(.df, col,indexer){
    group_col <- glue("{col}_group") %>% as.character()
    kw_col    <- glue("{col}_keywords") %>% as.character()
    out <- .df %>%
      tibble::add_column(!! kw_col := rep_len(list(), nrow(.))) %>%
      dplyr::mutate(!!group_col := NA_character_)

    for (i in 1:nrow(indexer)){
      kws <- indexer$keywords[[i]]
      locs <- indexer$match_locs[[i]]
      for (j in 1:length(kws)){
        kw <- kws[[j]]
        loc_list <- locs[[j]]
        out %<>%
          dplyr::mutate(!!kw_col := dplyr::if_else(
            loc_list,
            purrr::map(!!kw_col, function(x){
              return(purrr::splice(x, list(kw)))
              }), !!kw_col))
        out %<>%
          dplyr::mutate(!!group_col := dplyr::if_else(
            loc_list,
            indexer$title[[i]], !!group_col
          ))

      }
    }



    return(out)

  }
)

# split_column ======

Contextractr$set(
  "private", "split_column",
  function(col){
    span_sep <- private$span_sep
    sep      <- private$sep

    split_col <- col %>% rlang::as_list()

    # Split on sentences, or paragragraphs
    if (!is.na(span_sep)) split_col %<>% stringr::str_split(span_sep)
    # Split on word boundaries
    split_col %<>% purrr::map(~ stringr::str_split(., sep))
    return(split_col)
  }
)

# index_kws_in_strings ====

Contextractr$set(
  "private", "index_kws_in_strings",
  function(split_col, indexer){
    if (! "match_locs" %in% names(indexer)){
      stop("index_kws_in_strings called before find_keywords?")
    }
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
              # then into words
              # so split col is entry_list->sentence_list->word_list
              sentences <- split_col[match_locs]

              positions <- sentences %>%
                purrr::map_depth(2,~ agrep(keyword, ., max.distance = approx.match)) %>%
                purrr::set_names(nm = as.character(match_locs))
              return(positions)
            })}))
    return(out)
  }
)
