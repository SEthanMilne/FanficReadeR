GetComments <- function(link) {

  temp_commentdf <- list()

  work_page <- get_html(paste0(link, "?view_full_work=true"))

  stats <- work_page |>
    html_elements(css = ".stats dl dd")

  stats_elements <- stats |>
    html_attrs() |>
    unlist()

  if("comments" %in% stats_elements){
    comment_number <- stats |>
      html_elements(xpath = '//dd[@class="comments"]') |>
      html_text2() |>
      str_replace(fixed(","), "") |>
      as.numeric()
  } else {
    return(NA)
  }
  comment_pages <- ceiling(comment_number / 20) #only 20 comments per page


  comment_path <- work_page |>
    html_element("#show_comments_link a") |>
    html_attr("href") |>
    str_replace("hide_comments", "show_comments") #weird AO3 error

  temp_commentcount <- 0

  for (j in 1:comment_pages) {
    comment_url <-
      paste0("https://archiveofourown.org",
             comment_path,
             "&page=",
             j)

    comment_page <- get_html(comment_url)

    byline <- comment_page |>
      html_elements(".byline") |>
      html_text2() |>
      (\(x) x[2:length(x)])()

    date <- comment_page |>
      html_elements(".datetime") |>
      html_text2() |>
      (\(x) x[!str_detect(x, 'Last Edited')])()

    if (identical(date, character(0))){
      break #date being blank means no more comments -- end loop early
    }

    comments <- comment_page |>
      html_elements(xpath = '//*[@id="comments_placeholder"]//*[contains(@class, "userstuff")]') |>
      html_text2()

    comments_df <- cbind(byline, date) |>
      cbind(comments) |>
      data.frame() |>
      rename(author = byline,
             comment = comments) |>
      mutate(
        author = str_remove(author, date),
        author = str_remove(author, fixed("(orphan_account)")),
        comment = str_replace_all(comment, fixed("\n"), " "),
        date = gsub("^.{4}", "", date),
        date = anydate(date)
      ) |>
      mutate(
        chapter = ifelse(
          str_detect(author, "on Chapter"),
          str_extract(author, "on Chapter (\\d)+ "),
          "on Chapter 1"
        ),
        author = str_remove(author, paste0(" ", chapter)),
        chapter = str_remove(chapter, "on Chapter "),
        chapter = as.numeric(chapter)
      )

    temp_commentdf[[j]] <- comments_df

    temp_commentcount <- temp_commentcount + nrow(comments_df)
    if (temp_commentcount == comment_number)
    {break} #AO3 sometimes has >20 comments per page for unknown reason
  }

  do.call(rbind, temp_commentdf)
}
