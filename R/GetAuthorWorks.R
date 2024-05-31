GetAuthorWorks <- function(works_count, works_url){

  works_temp <- list()

  for (j in 1:ceiling(works_count/20)) {
    works_temp[[j]] <- GetAuthorWorksURLs(paste0(works_url, "?page=", j))
  }

  if (length(works_temp) == 1) {
    author_workurls <- data.frame(works_temp[[1]]) |>
      rename(work_urls = 1)
  } else {
    author_workurls <- data.frame(do.call(c, works_temp)) |>
      rename(work_urls = 1)
  }

  author_workurls
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
