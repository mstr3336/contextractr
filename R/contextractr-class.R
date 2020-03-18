#' Contextractr - Fuzzy Extraction and Classification by Context
#'
#' This R6 class aims to provide a simple interface for providing
#'      pattern + context based rules in order to extract values by
#'      keywords
#' @name Contextractr
#' @family Contextractr
#' @family Contextractr-important
NULL
#' @export
Contextractr <- R6::R6Class(
  "Contextractr",
  public = list(

  ),
  private = list(
    mapping = NULL,
    prefix_length = 4,
    suffix_length = 4,
    prefix_ignore = ".*\\.",
    suffix_ignore = "\\..*",
    span_sep = "[\\.\n]+",
    sep = "\\s+"
    #,L = NULL # Removed for now because I want to decouple functions that dont need state
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
#' @family Contextract-important
#' @importFrom magrittr %<>%
NULL

Contextractr$set(
  "public", "initialize",
  function(json = NULL, yaml = NULL, serial = NULL, tibble = NULL, name = NULL){
    `%||%` <- rlang::`%||%`

    name <- name %||% "default_name"
    #private$L <- logging::getLogger(glue::glue("contextractr.{name}"))

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
#'
#' Given a list-of-dicts style serial mapping, add this to the mapping rules
#'     of this set.
#'
#' @section Mapping Rules:
#' For a given entry, corresponding to a a row in a tibble, or a dict/named
#'     list in a list-of-dicts, the following must be specified:
#'
#'     * title: The output grouping for the item (string)
#'     * keywords: (list of strings) a list of keywords by which to identify
#'       this group
#'     * approx.match: (list of decimal numbers) (optional - sets to 0 if not set)
#'
#' @name Contextractr$add_serial
#' @family Contextractr
#' @importFrom magrittr %<>%
#' @param mapping the serial (list-of-dicts style) mapping to add
NULL

Contextractr$set(
  "public", "add_serial",
  function(mapping){
    mapping %>% private$serial_as_tbl() %>% private$add_mapping_entries()
    return(invisible(self))
  }
)

# add_json =======

#' Add a mapping
#'
#' Given a path to a mapping file, add that file's contents to the
#'      mapping
#' @inheritSection Contextractr$add_serial Mapping Rules
#' @name Contextractr$add_json
#' @family Contextractr
#' @param path the path to the mapping file
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
#'
#'
#' @name Contextractr$add_yaml
#' @inheritSection Contextractr$add_serial Mapping Rules
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

    group_col <- glue("{col}_groups") %>% as.character()
    kw_col    <- glue("{col}_keywords") %>% as.character()

    idx <- .df %>%
      dplyr::pull(col) %>%
      find_keywords(private$mapping)

    out <- .df %>% add_keyword_cols(col, idx)
    out %<>%
      dplyr::mutate(!!group_col := purrr::map(indexer, "title")) %>%
      dplyr::mutate_at(group_col, ~ purrr::map(., unique))
    out %<>%
      dplyr::mutate(!!kw_col    := purrr::map(indexer, "keywords")) %>%
      dplyr::select(-indexer)
    return(out)
  }
)

null_as_na <- function(x){
  if (is.null(x)) x <- NA
  return(x)
}

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
    missing <- mapping_colnames() %>% .[! . %in% nms]

    missing %<>%
      purrr::set_names() %>%
      purrr::map(~ NA_character_)

    out %<>%
      tibble::add_column(!!!missing) %>%
      dplyr::mutate_at(c("title"), as.character)
    return(out)
  }
)

# add_mapping_entries ====

#' @importFrom magrittr %<>%

Contextractr$set(
  "private", "add_mapping_entries",
  function(mappings){
    approx_filler <- function(kw, am){

      # If approx.match isn't set, it defaults to 0.0
      if (rlang::is_empty(am) | rlang::is_na(am)) am <- 0.0

      # If approx.match is given as a scalar, repeat that for all kw
      if (length(am) == 1) am %<>% rep_len(length.out = length(kw))

      # If approx.match was a missing, it's now defaulted everything to 0.0
      # If was a scalar, that's been repeated for all
      if (length(am) == length(kw)) return(am)
      msg <- glue::glue("For {pretty_string(kw)} invalid approx.match given!",
                        "length(approx.match) = {length(am)}",
                        "length(keywords)  = {length(kw)}")
      L$error("%s", msg)
      stop(msg)
    }

    mappings %<>%
      dplyr::mutate(approx.match = purrr::map2(keywords, approx.match,
                                                   approx_filler))


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

# find_keywords (GONE) =====


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
