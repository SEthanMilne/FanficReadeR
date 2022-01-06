GetWorkInfo <- function(input) {
  link <- WorkInfoURL(input)
  print(link)
  work <- get_html(link)

  raw_text <- work |>
    html_element(css = ".group") |>
    html_text2()

  work_info <- data.frame(matrix(nrow = 1, ncol = 0))
  work_info$word_count <- get_wordcount(raw_text)
  work_info$language <- get_language(raw_text)
  work_info$chapters <- get_chapters(raw_text)
  work_info$kudos <- get_kudos(raw_text)
  work_info$comments <- get_comments(raw_text)
  work_info$hits <- get_hits(raw_text)
  work_info$last_updated <- get_lastupdated(raw_text)
  work_info$completed <- get_completed(raw_text)
  work_info$status <- get_status(raw_text)
  work_info$bookmarks <- get_bookmarks(raw_text)
  work_info$relationships <- get_relationships(raw_text)
  work_info$warnings <- get_warnings(raw_text)
  work_info$age_category <- get_agecategory(raw_text)
  work_info$fandoms <- get_fandoms(raw_text)

  return(work_info)
}
