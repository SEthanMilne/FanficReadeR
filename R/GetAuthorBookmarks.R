GetAuthorBookmarks <- function(input) {
  ### Get bookmarks HTML Page
  bookmarks_html <- GET(
    paste0(
      "https://archiveofourown.org/users/",
      Author_Name(input),
      "/bookmarks"
    ),
    user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    )
  ) %>%
    read_html()

  bookmarks <- bookmarks_html %>%
    html_element(css = "#main") %>%
    html_text2()

  ### Setting up Works Table
  bookmarks_info <-
    sub(".*.Listing Works",
        "",
        gsub("",
             "",
             bookmarks)) %>%
    str_match_all("\\\n(.*?)by do not cache") %>%
    data.frame() %>%
    select(X2) %>%
    rowid_to_column() %>%
    rename(Titles = X2, Work = rowid)

  ### Works Page is a large string of text with all works data
  ### But Works are Preceded by the string "do not cache"
  ### Above function splits into DF by that string






  ## Check for bookmarks if they even exist

  if (0 == sub(".*Bookmarks *(.*?) *Collections.*",
               "\\1",
               gsub(
                 "\n",
                 " ",
                 gsub(
                   "[[:punct:]]",
                   "",
                   bookmarks_html %>%
                   html_element(css = "#dashboard") %>%
                   html_text2()
                 )
               ))) {
    "N/A"
  } else {
    bookmarks_info$raw_text <- strsplits(bookmarks,
                                         "by do not cache")[-1]

    ### Get Works IDs
    works_ids <- bookmarks_html %>%
      html_elements(css = "h4 a") %>%
      html_attr("href") %>%
      data.frame()

    colnames(works_ids) <- "works_ids"

    works_ids <- works_ids %>%
      filter(grepl("/works/", works_ids)) %>%
      mutate(works_ids = gsub("/works/", "", works_ids))

    bookmarks_info$work_id <- works_ids$works_ids

    ### Get Works URLs

    bookmarks_info$work_url <-
      paste0("https://archiveofourown.org/works/",
             bookmarks_info$work_id)



    ### Big For-Loop for capturing all Bookmarks Summary Data
    for (i in 1:nrow(bookmarks_info)) {
      ### Word Count
      bookmarks_info$word_count[i] <-
        str_match(bookmarks_info$raw_text[i], "Words\\:\\n(.*?)\\\n")[1, 2]

      ### Language
      bookmarks_info$language[i] <-
        str_match(bookmarks_info$raw_text[i], "Language\\:\\n(.*?)\\\n")[1, 2]

      ### Chapter Count
      bookmarks_info$chapters[i] <-
        sub("/.*",
            "",
            str_match(bookmarks_info$raw_text[i], "Chapters\\:\\n(.*?)\\\n")[1, 2])

      ### Kudos Count
      bookmarks_info$kudos[i] <-
        str_match(bookmarks_info$raw_text[i], "Kudos\\:\\n(.*?)\\\n")[1, 2]

      ### Comments Count
      bookmarks_info$comments[i] <-
        str_match(bookmarks_info$raw_text[i], "Comments\\:\\n(.*?)\\\n")[1, 2]

      ### Hits Count
      bookmarks_info$hits[i] <-
        str_match(bookmarks_info$raw_text[i], "Hits\\:\\n(.*?)\\\n")[1, 2]

      ### Last Updated
      bookmarks_info$last_updated[i] <-
        str_match(bookmarks_info$raw_text[i], "\\n(.*?)\\\n\\nTags")[1, 2]

      ### (In)Complete Status
      bookmarks_info$status[i] <-
        str_match(
          bookmarks_info$raw_text[i],
          paste0("\\n(.*?)\\\n\\n", bookmarks_info$last_updated[i])
        )[1, 2]

      ### Bookmarks Count
      bookmarks_info$bookmarks[i] <-
        str_match(bookmarks_info$raw_text[i], "Bookmarks\\:\\n(.*?)\\\n")[1, 2]

      ### Pairings (M/M, F/F, M/F, Multi, etc)
      bookmarks_info$pairings[i] <-
        str_match(bookmarks_info$raw_text[i],
                  paste0("\\n(.*?)\\\n", bookmarks_info$status[i]))[1, 2]

      ### Content Warnings
      bookmarks_info$warnings[i] <-
        str_match(bookmarks_info$raw_text[i],
                  paste0("\\n(.*?)\\\n", bookmarks_info$pairings[i]))[1, 2]

      ### Age Category (Mature, Teens, General)
      bookmarks_info$age_category[i] <-
        str_match(bookmarks_info$raw_text[i],
                  paste0("\\n(.*?)\\\n", bookmarks_info$warnings[i]))[1, 2]

      ### Fandoms
      bookmarks_info$fandoms[i] <-
        str_match(
          bookmarks_info$raw_text[i],
          paste0(
            "\\nFandoms\\:(.*?)\\\n",
            bookmarks_info$age_category[i]
          )
        )[1, 2]
    }

    bookmarks_info

  }
}
