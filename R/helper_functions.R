strsplits <- function(x, splits, ...)
{
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}


# Gets work URL from inputted string
WorkURL <- function(x){
  if (grepl("\\/chapters\\/", x )) {
    gsub(
      "\\/chapters\\/[0-9]*", "", x
    )
  } else {
    x
  }
}

WorkInfoURL <- function(input){
  paste0(WorkURL(input), "?view_adult=true")
}

# Gets chapter URL for inputted string
ChapterIndexURL <- function(input){
  paste0(WorkURL(input), "/navigate")
}

# Gets comment page URL for inputted string
CommentURL <- function(input, page){
  paste0(
    WorkURL(input),"?page=",
    page,
    "&show_comments=true&view_adult=true&view_full_work=true#comments"
  )
}


get_authorname <- function(input){
  get_html(input) |>
    html_elements(css = ".heading") |>
    html_text2() |>
    data.frame() |>
    (\(x) x[6,])() |>
    (\(x) strsplits(x, ", "))()
}
