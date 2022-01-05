Author_Name <- function(input) {
  if (grepl("https://archiveofourown.org/users", input, fixed = FALSE)) {
    strsplit(gsub("https://archiveofourown.org/users/", "", input),
             "[/]")[[1]][1]

  } else {
    if (grepl("https://archiveofourown.org/", input, fixed = FALSE)) {
      print("Please enter a valid AO3 user profile link OR user name")

    } else{
      input
    }
  }

}

get_profileurl <- function(x) {
  paste0("https://archiveofourown.org/users/",
         Author_Name(x),
         "/profile")
}

get_worksurl <- function(x) {
  paste0("https://archiveofourown.org/users/",
         Author_Name(x),
         "/works")
}

get_authorprofile <- function(x){
  get_html(get_profileurl(x))
}

get_authorworks <- function(x){
  get_html(get_worksurl(x))
}

get_joindate <- function(x) {
  sub(".*I joined on *(.*?) *My user.*",
      "\\1", gsub("\n", " ",
                  gsub("[[:punct:]]", "", x)))
}

get_userid <- function(x) {
  str_extract(gsub("\n", " ",
                   gsub("[[:punct:]]", "", x)), "My user ID is*.+") |>
    str_replace("My user ID is ", "")
}

get_workscount <- function(x) {
  sub(".*Works *(.*?) *Series.*", "\\1",
      gsub(
        "\n",
        " ",
        gsub(
          "[[:punct:]]",
          "",
          x |>
            html_element(css = "#dashboard") |>
            html_text2()
        )
      ))
}

get_bookmarkscount <- function(x) {
  sub(".*Bookmarks *(.*?) *Collections.*",
      "\\1",
      gsub(
        "\n",
        " ",
        gsub(
          "[[:punct:]]",
          "",
          x |>
            html_element(css = "#dashboard") |>
            html_text2()
        )
      ))
}

get_bookmarksurl <- function(x){
  paste0("https://archiveofourown.org/users/",
         Author_Name(x),
         "/bookmarks")
}
