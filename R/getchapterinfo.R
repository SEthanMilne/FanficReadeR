GetChapterIndex <- function(input) {
  link <- paste0(input, "/navigate")
  index <- get_html(link)

  chapter_info <- index |>
    html_element(css = "#main") |>
    html_text2() |>
    strsplits("\n") |>
    data.frame()

  colnames(chapter_info) <- "title"

  end <- nrow(chapter_info)
  chapter_info <- chapter_info[2:end, 1] |>
    data.frame()

  colnames(chapter_info) <- "title"

  chapter_links <- get_chapterurls(index)

  chapter_info <- chapter_info |>
    mutate(
      chapter = chapter_getnumber(title),
      date = chapter_getdate(title),
      title = chapter_cleantitle(title),
      chapter_links = chapter_links$url
    )

  chapter_info
}

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


