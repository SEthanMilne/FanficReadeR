GetAuthorInfo <- function(input) {
  ### Setup Author Info Table
  author_info <- data.frame(matrix(ncol = 8, nrow = 1))

  names(author_info) <- c(
    "Name",
    "ID",
    "JoinDate",
    "ProfileURL",
    "NumberWorks",
    "WorksURL",
    "NumberBookmarks",
    "BookmarksURL"
  )


  ### Download Profile HTML Page
  author_profile <- get_authorprofile(input)

  profile <- author_profile |>
    html_element(css = ".meta") |>
    html_text2()

  author_info$Name <- Author_Name(input)
  author_info$ID <- get_userid(profile)
  author_info$JoinDate <- get_joindate(profile)
  author_info$ProfileURL <- get_profileurl(input)
  author_info$NumberWorks <- get_workscount(author_profile)
  author_info$WorksURL <- get_worksurl(input)
  author_info$NumberBookmarks <- get_bookmarkscount(author_profile)
  author_info$BookmarksURL <- get_bookmarksurl(input)

  author_info
}
