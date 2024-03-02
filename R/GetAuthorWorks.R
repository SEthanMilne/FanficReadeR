GetAuthorWorks <- function(input) {
  author_works <- get_html(input)

  workslist <- author_works |>
    html_element("#main") |>
    html_elements(".work") |>
    html_attr("id") |>
    str_remove_all(fixed("work_")) |>
    na.omit()

  author_works_info <- list()

  for (j in 1:length(workslist)) {
    link <- paste0("https://archiveofourown.org/works/", workslist[j])
    author_works_info[[link]] <- GetWorkInfo(link)
  }

  author_works <- do.call(rbind, author_works_info)

  author_works
}

GetAuthorWorksURLs <- function(input) {
  author_works <- get_html(input)

  workslist <- author_works |>
    html_element("#main") |>
    html_elements(".work") |>
    html_attr("id") |>
    str_remove_all(fixed("work_")) |>
    na.omit()

  author_works_urls <- c()

  for (j in 1:length(workslist)) {
    link <- paste0("https://archiveofourown.org/works/", workslist[j])
    author_works_urls <- append(author_works_urls, link)
  }

  author_works_urls
}
