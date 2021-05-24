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
