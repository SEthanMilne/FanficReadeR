GetAuthorInfo <- function(input) {

  ### Get author name
  author_name <- author_name(input)

  ### Skip over deleted author accounts
  if (author_name == "orphan_account"){
    return(NA)
  }

  ### Otherwise, get author url, html page, sub-pages
  author_url <- get_profileurl(author_name)
  author_profile <- get_html(author_url)
  profile_stats <- author_profile |>
    html_element(".meta") |>
    html_text2()
  author_dashboard <- author_profile |>
    html_elements("#dashboard")

  ### Setup Author Info Table
  author_info <- data.frame(matrix(ncol = 0, nrow = 1))

  ###### Basic author info
  author_info$name <- author_name
  author_info$author_url <- author_url
  author_info$bookmarks_url <- get_authorbookmarksurl(author_name)
  author_info$works_url <- get_authorworksurl(author_name)


  ###### Author bio info
  author_info$join_date <- get_joindate(profile_stats) |>
    as.Date()
  author_info$bio <- author_profile |>
    html_elements(".module p") |>
    html_text2() |>
    paste(collapse = "") |>
    str_replace_all(fixed("\n"), " ")

  ##### Author stats
  author_info$works_count <- author_dashboard |>
    html_text2() |>
    get_workscount() |>
    as.numeric()

  author_info$bookmarks_count <- author_dashboard |>
    html_text2() |>
    get_bookmarkscount() |>
    as.numeric()


  author_info
}

author_name <- function(input) {

  # Return first author name
  temp_name <- gsub(",.*$", "", input)

  # If there are pseudonyms in play, return bracketed name
  if (str_detect(temp_name, fixed("("))){
    temp_name <- gsub("\\(([^()]*)\\)|.", "\\1", temp_name, perl=T)
    temp_name
  } else{
    temp_name
  }

}

get_profileurl <- function(input) {
  paste0("https://archiveofourown.org/users/",
         input,
         "/profile")
}

get_worksurl <- function(input) {
  paste0("https://archiveofourown.org/users/",
         input,
         "/works")
}

get_authorbookmarksurl <- function(input){
  paste0("https://archiveofourown.org/users/",
         input,
         "/bookmarks")
}

get_authorworksurl <- function(input){
  paste0("https://archiveofourown.org/users/",
         input,
         "/works")
}

get_joindate <- function(x) {
  sub(".*I joined on *(.*?) *My user.*",
      "\\1", x) |>
    str_remove_all(fixed("\n")) |>
    str_remove(fixed(":"))
}

get_workscount <- function(x) {
  sub(".*Works *(.*?) *Series.*", "\\1",
      gsub("\n",
           " ",
           gsub("[[:punct:]]",
                "",
                x)))
}

get_bookmarkscount <- function(x) {
  sub(".*Bookmarks *(.*?) *Collections.*",
      "\\1",
      gsub("\n",
           " ",
           gsub("[[:punct:]]",
                "",
                x)))
}
