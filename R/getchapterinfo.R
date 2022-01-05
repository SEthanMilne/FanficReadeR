GetChapterInfo <- function(input) {
  index <- get_html(ChapterIndexURL(input))

  chapter_info <- index |>
    html_element(css = "#main") |>
    html_text2() |>
    strsplits("\n") |>
    data.frame()

  colnames(chapter_info) <- "title"

  end <- nrow(chapter_info) - 1
  chapter_info <- chapter_info[2:end, 1] |>
    data.frame()

  colnames(chapter_info) <- "title"

  chapter_info <- chapter_info |>
    mutate(
      chapter = chapter_getnumber(title),
      date = chapter_getdate(title),
      title = chapter_cleantitle(title)
    )


  links <- get_chapterurls(index)
  links$wordcount <- 0

  for (i in 1:nrow(links)) {
    url <- paste0(links[i, 1], "?view_adult=true")
    wordcount <- get_html(url) |>
      html_element(css = "div.userstuff.module") |>
      html_text2() |>
      str_replace_all("\\n", " ") |>
      wordcount()
    links[i, 2] <- wordcount
  }

  chapter_info <- cbind(chapter_info, links)

  chapter_info
}
