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
