GetAuthorInfo <- function(input) {
  ### Setup Author Info Table
  author_info <- data.frame(matrix(ncol = 2, nrow = 8)) %>%
    rename(Category = X1, Value = X2)

  author_info$Category <-
    c(
      "Name",
      "ID",
      "Join Date",
      "Profile URL",
      "Number of Works",
      "Works URL",
      "Number of Bookmarks",
      "Bookmarks URL"
    )

  ### Get Author Name from URL
  author_info[1, 2] <- Author_Name(input)

  ### Get Profile URL
  author_info[4, 2] <-
    paste0("https://archiveofourown.org/users/",
           author_info[1, 2],
           "/profile")

  ### Get Works URL
  author_info[6, 2] <-
    paste0("https://archiveofourown.org/users/",
           author_info[1, 2],
           "/works")

  ### Download Profile HTML Page
  author_profile <- GET(
    author_info[4, 2],
    user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    )
  ) %>%
    read_html()

  profile <- author_profile %>%
    html_element(css = ".meta") %>%
    html_text2()

  ### Get Join Date
  author_info[3, 2] <-
    sub(".*I joined on *(.*?) *My user.*", "\\1", gsub("\n", " ",
                                                       gsub("[[:punct:]]", "", profile)))

  ### Get User ID
  author_info[2, 2] <- str_extract(gsub("\n", " ",
                                        gsub("[[:punct:]]", "", profile)), "My user ID is*.+") %>%
    str_replace("My user ID is ", "")



  ### Get Works Count
  author_info[5, 2] <- sub(".*Works *(.*?) *Series.*", "\\1",
                           gsub(
                             "\n",
                             " ",
                             gsub(
                               "[[:punct:]]",
                               "",
                               author_profile %>%
                                 html_element(css = "#dashboard") %>%
                                 html_text2()
                             )
                           ))

  ### Get Bookmark count
  author_info[7, 2] <- sub(".*Bookmarks *(.*?) *Collections.*",
                           "\\1",
                           gsub(
                             "\n",
                             " ",
                             gsub(
                               "[[:punct:]]",
                               "",
                               author_profile %>%
                                 html_element(css = "#dashboard") %>%
                                 html_text2()
                             )
                           ))

  ### Get Bookmarks URL
  author_info[8, 2] <-
    paste0("https://archiveofourown.org/users/",
           author_info[1, 2],
           "/bookmarks")

  author_info
}
