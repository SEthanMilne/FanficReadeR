GetChapterText <- function(input) {
  chapter_text <- data.frame(matrix(nrow = 1, ncol = 0))

  html <- get_html(paste0(input, "?view_adult=true"))

  chapter_number <- html |>
    html_elements(css = '.title') |>
    (\(x) x[2])() |>
    html_text2() |>
    stringi::stri_extract_first_regex("[0-9]+")

  chapter_text$chapter <- chapter_number

  chapter_text$body <- html |>
    html_element(css = "div.userstuff.module") |>
    html_text2() |>
    str_replace_all("\\n", " ") |>
    str_replace(stringr::fixed("Chapter Text "), "")


  chapter_text$summary <- html |>
    html_element(css = ".summary") |>
    html_text2() |>
    str_replace_all("\\n", " ") |>
    str_replace(stringr::fixed("Summary:  "), "")

  chapter_text$notes <- html |>
    html_element(css = ".notes") |>
    html_text2() |>
    str_replace_all("\\n", " ") |>
    str_remove(fixed("Notes:  ")) |>
    str_remove(fixed("(See the end of the work for notes.)")) |>
    str_remove(fixed("(See the end of the work for more notes.)")) |>
    str_remove(fixed("(See the end of the chapter for notes.)"))




  if (as.numeric(chapter_number) == 1) {
    chapter_text$endnotes <- html |>
      html_element(css = "#work_endnotes .userstuff") |>
      html_text2() |>
      str_replace_all(fixed("\n"), " ")
  } else {
    chapter_text$endnotes <- html |>
      html_element(css = paste0("#chapter_", chapter_number, "_endnotes")) |>
      html_text2() |>
      str_remove(fixed("Notes:\n\n")) |>
      str_replace_all(fixed("\n"), " ")
  }

  chapter_text
}
