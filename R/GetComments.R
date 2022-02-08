GetComments <- function (input) {

  ### Placeholder DF to store results
  comment_data <- matrix(ncol = 4, nrow = 1) |>
    data.frame() |>
    rename(
      user = X1,
      chapter = X2,
      date = X3,
      text = X4
    )


  for (i in 1:1000) {
    url <- CommentURL(input, i)
    comm <- get_html(url)

    if (comm |>
        html_element(xpath = '//*[@id="feedback"]/p') |>
        html_text2() |>
        is.na() != TRUE){
      return(0)
    }


    comments <- comm |>
      html_elements(css = "ol li") |>
      html_text2() |>
      data.frame()

    names(comments) <- "comments"

    if (0 ==
        comments |>
        filter(grepl("on Chapter", comments)) |>
        nrow()) {
      break
    } else {

      names(comments) <- "comments"

      ### Extracts user, chapter, date, and text of comment
      comments <- comments |>
        filter(grepl("on Chapter", comments)) |>
        mutate(user = comments_getuser(comments)) |>
        mutate(chapter = comments_getchapter(comments)) |>
        mutate(date = comments_getdate(comments, chapter)) |>
        mutate(text = comments_gettext(comments,date)) |>
        select(-comments) |>
        distinct(text, .keep_all = TRUE) ### Removes duplicates


      comment_data <- rbind(comment_data, comments)
    }
  }


  comment_data <- comment_data[-1, ] #|>

  comment_data

}




GetComments_1chap <- function(input) {
  id <- sub(".*https://archiveofourown.org/works/", "", input)
  comment_url <-
    paste0("https://archiveofourown.org/comments/show_comments?work_id=",
           id)
  comm <- get_html(comment_url)

  comment_text <- comm |>
    html_element(xpath = '//*[@id="outer"]') |>
    html_element(xpath = '//*[@id="inner"]') |>
    html_element(xpath = '//*[@id="main"]') |>
    html_element(xpath = '//*[@id="feedback"]') |>
    html_element(xpath = '//*[@id="comments_placeholder"]') |>
    html_text2()

  if (comment_text == ""){
    return("Comments Hidden")
  }

  comment_text <- gsub("Parent Thread\n", "", comment_text)
  comment_text <- gsub("(Previous comment deleted.)\n\n", "", comment_text)

  comment_df <-
    strsplits(comment_text, "\n\nComment Actions\nReply\nThread\n") |>
    data.frame()

  names(comment_df) <- "raw_text"

  comment_data <- comment_df |>
    (\(x) slice(x, 1:(n() - 1)))() |>
    mutate(raw_text = str_remove(raw_text, "\\(Previous comment deleted.\\)\\n\\n")) |>
    mutate(raw_text = str_remove(raw_text, "\\n\\((.*?)this thread\\)\\n\\n")) |>
    mutate(user = gsub(" .*$", "", raw_text)) |>
    mutate(text = sub(".*\\n\\n", "", raw_text)) |>
    mutate(raw_text = str_remove(raw_text, fixed(user))) |>
    mutate(raw_text = str_replace(raw_text, "\\n\\n", " COMMENTSTARTS ")) |>
    mutate(date = sub("COMMENTSTARTS.*", "", raw_text)) |>
    mutate(date = str_remove(date, "\\((.*?)\\)")) |>
    select(-raw_text)


  comment_data
}
