library(testthat)
library(contextractr)
context("Can a simple string be classified by the corpus?")

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

compare_helper <- function(actual_df, expect_df, by = "EVENT_KEY",
                           cols = c("note_value_group", "note_value_keywords")){
  compare <- actual_df %<>%
    dplyr::inner_join(expect_df, by = by, suffix = c("", ".e"))

  for (col in cols){
    a <- compare %>% dplyr::pull(col)
    e <- compare %>% dplyr::pull(glue::glue("{col}.e"))

    expect_identical(a,e, info = glue::glue("Does {col} match?",
                                            "{info_comp(a,e)}", .sep = "\n"))
  }
}


test_that("Can classify some simple text strings correctly", {
  input <- example_simple()
  ctx <- Contextractr$new(json = "./chest_pain.json")
  out <- input %>% ctx$locate_keywords(note_value)

  e <- tibble::tribble(
    ~EVENT_KEY, ~note_value_group, ~note_value_keywords,
    1         , "chest pain"     , list("chest pain"),
    2         , "chest pain"     , list("chest pain", "cp"),
    3         , NA               , NULL
  )

  compare_helper(out, e, by = "EVENT_KEY")
})

context("Does fuzzy matching work correctly?")

test_that("Can match on fuzzy", {
  input <- example_fuzzy()
  ctx <- Contextractr$new(json = ".chest_pain.json")

  out <- input %>% ctx$locate_keywords(note_value)


  compare_helper(out, e)
})

context("Classification based on prefixes/suffixes works correctly")

test_that("Can use prefixes/suffixes correct",{
  skip()
  input <- example_longer()
  ctx <- Contextractr$new(json = "./two_terms.json")
  out <- input %>% ctx$locate_keywords(note_value)


  e <- list()
  e[[1]] <- eg_expect_simple() %>% dplyr::filter(EVENT_KEY != 3)
  e[[2]] <- tibble::tribble(
    ~ EVENT_KEY, ~CLASSIFICATION, ~CLASSIFICATION_REASON,
    3          , "3"              , "likes",
    4          , "1"              , "hates",
    5          , "3,4"            , "likes,really likes",
    6          , "4"              , "fantastic"
  )
  e %<>% dplyr::bind_rows()

  compare_helper(out, e)
})

