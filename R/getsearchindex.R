GetSearchIndex <- function(fandom, pages, start = 1, date_from="", date_to="", complete="") {
  indexURL <- get_searchURL(fandom, date_from, date_to, complete)
  index <- get_html(indexURL)
  indexpages <- get_indexpages(index)

  pagecount <- min(pages, indexpages)

  index_ids <- as.data.frame(matrix(ncol = 1, nrow = 0))

  for (i in start:pagecount){
    pageURL <- get_searchURLpage(fandom, i, date_from, date_to, complete)

    page_html <- get_html(pageURL)

    id_list <- get_pageworkIDs(page_html)

    index_ids <- rbind(index_ids, id_list)
  }

  index_ids
}
