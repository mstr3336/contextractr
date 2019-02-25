context("Can a simple string be classified by the corpus?")

`%<>%` <- magrittr::`%<>%`

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
  strings[[2]] <- "Left sided chest pain"
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

eg_expect_simple <- function(){
  out <- tibble::tribble(
    ~EVENT_KEY, ~CLASSIFICATION, ~CLASSIFICATION_REASON,
    1         , "HIGH"         , "left sided cp,radiating cp",
    2         , "HIGH"         , "left sided cp",
    3         , "Unclassified" , ""
  )
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

compare_helper <- function(actual_df, expect_df){
  compare <- actual_df %<>%
    dplyr::inner_join(expect_df, by = "EVENT_KEY", suffix = c("", ".e"))

  for (col in c("CLASSIFICATION", "CLASSIFICATION_REASON")){
    a <- compare %>% dplyr::pull(col)
    e <- compare %>% dplyr::pull(glue::glue("{col}.e"))

    expect_identical(a,e, info = glue::glue("Does {col} match?",
                                            "{info_comp(a,e)}", .sep = "\n"))
  }
}


test_that("Can classify some simple text strings correctly", {
  input <- example_simple()
  out <- input %>% Classify_Text("note_value", "./chest_pain.json")

  expected <- eg_expect_simple()

  compare_helper(out, expected)
})

context("Does fuzzy matching work correctly?")

test_that("Can match on fuzzy", {
  input <- example_fuzzy()

  out <- input %>% Classify_Text("note_value","./chest_pain.json")

  e <- eg_expect_simple()
  e[e$EVENT_KEY == 2, "CLASSIFICATION"][[1]] <- "Unclassified"
  e[e$EVENT_KEY == 2, "CLASSIFICATION_REASON"][[1]] <- ""

  compare_helper(out, e)
})

context("Classification based on prefixes/suffixes works correctly")

test_that("Can use prefixes/suffixes correct",{
  input <- example_longer()
  out <- input %>% Classify_Text("note_value", "./two_terms.json")


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





