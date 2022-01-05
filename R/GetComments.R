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
    }
    else{

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

  ### Get Authors Name
  name <- comm |>
    html_elements(css = ".heading") |>
    html_text2() |>
    data.frame()

  name <- name[6,]

  ### Flag comments made by the author
  comment_data <- comment_data[-1, ] #|>
  #mutate(author = as.character(str_detect(name, user)))

  comment_data[-1, ]

}
