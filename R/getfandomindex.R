GetFandomIndex <- function(fandom, pages) {
  indexURL <- get_indexURL(fandom)
  index <- get_html(indexURL)
  indexpages <- get_indexpages(index)

  pagecount <- min(pages, indexpages)

  index_ids <- as.data.frame(matrix(ncol = 1, nrow = 0))

  for (i in 1:pagecount){
    pageURL <- get_indexURLpage(indexURL, i)

    page_html <- get_html(pageURL)

    id_list <- get_pageworkIDs(page_html)

    index_ids <- rbind(index_ids, id_list)
  }

  index_ids
}
