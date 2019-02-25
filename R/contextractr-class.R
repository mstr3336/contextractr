


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
    suffix_ignore = "\\..*"
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
      if (!rlang::is_empty(inputs[[nm]])) inputs[[nm]] %>% handlers[[nm]]
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
    mapping %>% private$serial_to_tbl() %>% private$add_mapping_entries()
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
    out %<>% dplyr::mutate_at(missing, ~ NA)
    return(out)
  }
)

# add_mapping_entries ====

#' @importFrom magrittr %<>%

Contextractr$set(
  "private", "add_mapping_entries",
  function(mappings){
    # Definitely do some validation


    private$mapping %<>% dplyr::bind_rows(mapping)
    return(invisible(self))
  }
)
