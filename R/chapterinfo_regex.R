get_chapterurls <- function(x){
  links <- x |>
    html_elements(css = "#main ol a") |>
    html_attr("href") |>
    data.frame()

  names(links) <- "url"

  links <- links |>
    mutate(url = paste0("https://archiveofourown.org", url))

  links
}

chapter_getnumber <- function(x){
  str_match(x, "^(.*?)\\.")[, 2]
}

chapter_getdate <- function(x){
  str_match(x, "\\((.*?)\\)$")[, 2]
}

chapter_cleantitle <- function(x) {
  gsub("\\s*\\([^\\)]+\\)", "",
       gsub("^.*\\.", "", x))
}
