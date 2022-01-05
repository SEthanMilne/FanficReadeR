get_indexURL <- function(input) {
  paste0(
    "https://archiveofourown.org/tags/",
    str_replace_all(
      str_replace_all(input, " ", "%20"), "\\.", "*d*"),
    "/works"
  )
}

get_indexURLpage <- function(input, page) {
  paste0(input, "?page=", page)
}

get_indexpages <- function(input) {
  input |>
    html_element(css = "ol.pagination.actions") |>
    html_text2() |>
    str_match("\\n(.*?)\\nNext") |>
    (\(x) x[,2])() |>
    as.numeric()
}

get_pageworkIDs <- function(input){
  input |>
    html_elements(css = "ol.work.index.group") |>
    as.character() |>
    str_match_all("<li id=(.*?) class=") |>
    data.frame() |>
    select("X2") |>
    mutate(X2 = gsub('[[:punct:]]', "", X2),
           X2 = gsub("work", "", X2)) |>
    rename(id = X2) |>
    mutate(id = as.numeric(id),
           id = paste0("https://archiveofourown.org/works/", id))
}
