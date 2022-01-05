GetAuthorWorks <- function(input) {
  author_works <- get_html(get_worksurl(input))
  works_info <- get_workstable(author_works)

  if (check_if_noworks(author_works)) {
    "N/A"
  }
  else{


    works_info$raw_text <- strsplits(author_works %>%
                                       html_element(css = "#main") %>%
                                       html_text2(),
                                     works_info$Titles)[-1]


    ### Get Works IDs and URLs
    works_ids <- author_works %>%
      html_elements(css = "h4 a") %>%
      html_attr("href") %>%
      data.frame()

    colnames(works_ids) <- "works_ids"

    works_ids <- works_ids %>%
      filter(grepl("/works/", works_ids)) %>%
      mutate(works_ids = gsub("/works/", "", works_ids))

    works_info$work_id <- works_ids$works_ids


    ### Get Works URLs
    works_info$work_url <-paste0("https://archiveofourown.org/works/",
                                 works_info$work_id)

    for (i in 1:nrow(works_info)) {
      raw_text <- works_info$raw_text[i]
      works_info$word_count[i] <- get_wordcount(raw_text)
      works_info$language[i] <- get_language(raw_text)
      works_info$chapters[i] <- get_chapters(raw_text)
      works_info$kudos[i] <- get_kudos(raw_text)
      works_info$comments[i] <- get_comments(raw_text)
      works_info$hits[i] <- get_hits_alt(raw_text)
      works_info$last_updated[i] <- get_lastupdated_alt(raw_text)
      works_info$status[i] <- get_status_alt(raw_text)
      works_info$bookmarks[i] <- get_bookmarks(raw_text)
      works_info$pairings[i] <- get_relationships_alt(raw_text)
      works_info$warnings[i] <- get_warnings_alt(raw_text)
      works_info$age_category[i] <- get_category_alt(raw_text)
      works_info$fandoms[i] <- get_fandoms_alt(raw_text)
    }

    works_info |>
      select(-raw_text)

  }
}
