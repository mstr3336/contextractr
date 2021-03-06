library(testthat)
library(contextractr)


# Produce a more informative output when failing expect_equivalent/identical
# testcases for visual comparison
info_comp <- function(actual, expect){
  if (!requireNamespace("glue", quietly = TRUE)){
    return()
  }
  out <- glue::glue("Expect: {expect}",
                    "Actual: {actual}", .sep = "\n")
  return(out)
}


example_simple <- function(){
  strings <- list()
  strings[[1]] <- glue::glue(
    "Left sided chest pain radiating to jaw, heavy and sharp in nature,",
    "nausa, mild SOB. Aspirin 300mgs, 900mcg S/L GTN, pain initially 6 now 3"
  )
  strings[[2]] <- "Left sided cp and also a bit of chest pain"
  strings[[3]] <- "just likes hospital!!!"

  strings %<>% purrr::map(as.character)

  out <- tibble::tribble(
    ~ENCNTR_KEY, ~EVENT_KEY, ~note_value,
    1          , 1         ,  strings[[1]],
    2          , 2         ,  strings[[2]],
    3          , 3         ,  strings[[3]]
  )
  return(out)
}

eg_expect_simple <- function(){
  e <- tibble::tribble(
    ~EVENT_KEY, ~note_value_groups, ~note_value_keywords,
    1         , c("chest pain")     , c("chest pain"),
    2         , c("chest pain")     , c("chest pain", "cp"),
    3         , NULL                , NULL
  )
  return(e)
}

example_longer <- function(){
  out <- example_simple()

  more <- tibble::tribble(
    ~ENCNTR_KEY, ~EVENT_KEY, ~note_value,
    4          , 4         , "just hates hospital",
    5          , 5         , "really likes hospital",
    6          , 6         , "thinks hospital is fantastic"
  )
  return(dplyr::bind_rows(out,more))
}

example_fuzzy <- function(){
  out <- example_simple()

  sr <- stringr::str_replace

  out %<>%
    dplyr::mutate(note_value = dplyr::case_when(
      EVENT_KEY == 1 ~ sr(note_value, "pain", "pan"),
      EVENT_KEY == 2 ~ sr(note_value, "chest pain", "choo poo"),
      TRUE ~ note_value
    ))
  return(out)
}


compare_helper <- function(actual_df, expect_df, by = "EVENT_KEY",
                           cols = c("note_value_groups", "note_value_keywords")){
  compare <- actual_df %<>%
    dplyr::inner_join(expect_df, by = by, suffix = c("", ".e"))

  for (col in cols){
    a <- compare %>% dplyr::pull(col)
    e <- compare %>% dplyr::pull(glue::glue("{col}.e"))

    expect_identical(a,e, info = glue::glue("Does {col} match?",
                                            "{info_comp(a,e)}", .sep = "\n"))
  }
}

context("Can presence of keywords be found?")
test_that("Can classify some simple text strings correctly", {
  input <- example_simple()
  ctx <- Contextractr$new(json = "./chest_pain.json")
  out <- input %>% ctx$locate_keywords(note_value)

  e <- eg_expect_simple()

  compare_helper(out, e, by = "EVENT_KEY")
})

context("Does fuzzy matching work correctly?")

test_that("Can match on fuzzy", {
  input <- example_fuzzy()
  ctx <- Contextractr$new(json = "./chest_pain.json")

  out <- input %>% ctx$locate_keywords(note_value)

  e <- tibble::tribble(
    ~EVENT_KEY, ~note_value_groups, ~note_value_keywords,
    1         , c("chest pain")  , c("chest pain"),
    2         , c("chest pain")  , c("cp"),
    3         , NULL             , NULL
  )

  compare_helper(out, e)
})

context("Classification based on prefixes/suffixes works correctly")

test_that("Can use prefixes/suffixes correct",{
  #skip("Not fully written")
  input <- example_longer()
  ctx <- Contextractr$new(json = "./two_terms.json")
  out <- input %>% ctx$locate_keywords(note_value)

  out_cols <- c("note_value_groups", "note_value_keywords")

  e <- list()
  e[[1]] <- eg_expect_simple() %>%
    dplyr::filter(EVENT_KEY != 3) %>%
    dplyr::mutate_at(out_cols, as.list)

  e[[2]] <- tibble::tribble(
    ~ EVENT_KEY, ~note_value_groups, ~note_value_keywords,
    3          , "3"              , "likes",
    4          , "1"              , "hates",
    5          , c("3,4")         , c("likes","really likes"),
    6          , "4"              , "fantastic"
  ) %>% dplyr::mutate_at(out_cols, as.list)

  e %<>% dplyr::bind_rows()

  out %<>% dplyr::select_at(c("EVENT_KEY", out_cols))

  compare_helper(out, e)
})

context("Default values work correctly")

test_that("Don't need to specify approx.match",{

  input <- example_simple()
  ctx <- Contextractr$new(yaml = "./no_approx.yaml")

  out <- input %>% ctx$locate_keywords(note_value)

  e <- eg_expect_simple()

  compare_helper(out, e)

})
