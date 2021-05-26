GetWorkInfo <- function(input){
  work <- GET(
    input,
    user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    )
  ) %>%
    read_html()

  raw_text <- work %>%
    html_element(css = ".group") %>%
    html_text2()

  work_info <- data.frame(matrix(nrow = 1, ncol = 0))

  ### Word Count
  work_info$word_count <-
    str_match(raw_text, "Words\\:\\n(.*?)\\\n")[, 2]

  ### Language
  work_info$language <-
    str_match(raw_text, "Language\\:\\n(.*?)\\\n")[, 2]

  ### Chapter Count
  work_info$chapters <-
    sub("/.*",
        "",
        str_match(raw_text, "Chapters\\:\\n(.*?)\\\n")[, 2])

  ### Kudos Count
  work_info$kudos <-
    str_match(raw_text, "Kudos\\:\\n(.*?)\\\n")[, 2]

  ### Comments Count
  work_info$comments <-
    str_match(raw_text, "Comments\\:\\n(.*?)\\\n")[, 2]

  ### Hits Count
  work_info$hits <-
    str_match(gsub("\n", " ", raw_text), "Hits\\:(.*)")[, 2]

  ### Last Updated
  work_info$last_updated <-
    str_match(raw_text, "Updated\\:\\n(.*?)\\\n")[, 2]

  ### (In)Complete Status
  work_info$status <- if ("?" == sub(".*/",
                                     "",
                                     str_match(raw_text, "Chapters\\:\\n(.*?)\\\n")[, 2])) {
    "Ongoing"
  } else {
    "Complete"
  }

  ### Bookmarks Count
  work_info$bookmarks <-
    str_match(raw_text, "Bookmarks\\:\\n(.*?)\\\n")[, 2]

  ### Relationships
  work_info$relationships <-
    str_match(gsub("\n", " ", raw_text), "Relationships\\:(.*?)Characters")[, 2]

  ### Content Warnings
  work_info$warnings <-
    str_match(raw_text, "Archive Warning\\:\\n(.*?)\\\n")[, 2]

  ### Age Category (Mature, Teens, General)
  work_info$age_category <-
    str_match(raw_text, "Rating\\:\\n(.*?)\\\n")[, 2]

  ### Fandoms
  work_info$fandoms <-
    str_match(gsub("\n", " ", raw_text), "Fandoms\\:(.*?)Relationships")[, 2]

  ### Display the work info table
  work_info
}
