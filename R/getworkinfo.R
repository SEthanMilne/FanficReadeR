GetWorkInfo <- function(input) {
  link <- paste0(input, "?view_adult=true")
  work <- get_html(link)

  stats <- work |>
    html_elements(css = ".stats dl dd")

  stats_elements <- stats |>
    html_attrs() |>
    unlist()

  work_info <- data.frame(matrix(nrow = 1, ncol = 0))

  work_info$title <- work |>
    html_elements(".heading") |>
    html_elements(xpath = "//h2[@class='title heading']") |>
    html_text2()

  work_info$author <- work |>
    html_elements(".heading") |>
    html_elements(xpath = "//h3[@class='byline heading']") |>
    html_text2()

  work_info$word_count <- stats |>
    html_elements(xpath = '//dd[@class="words"]') |>
    html_text2() |>
    str_replace(fixed(","), "") |>
    as.numeric()

  work_info$language <- work |>
    html_elements(css = ".language") |>
    html_text2() |>
    (\(x)x[2])()

  work_info$chapters <- stats |>
    html_elements(xpath = '//dd[@class="chapters"]') |>
    html_text2() |>
    str_replace(fixed("/?"), "") |>
    (\(x) sub("/.*",
              "", x))() |>
    as.numeric()


  if("kudos" %in% stats_elements){
    work_info$kudos <- stats |>
      html_elements(xpath = '//dd[@class="kudos"]') |>
      html_text2() |>
      str_replace(fixed(","), "") |>
      as.numeric()
  } else {
    work_info$kudos <- 0
  }

  if("comments" %in% stats_elements){
    work_info$comments <- stats |>
      html_elements(xpath = '//dd[@class="comments"]') |>
      html_text2() |>
      str_replace(fixed(","), "") |>
      as.numeric()
  } else {
    work_info$comments <- 0
  }

  work_info$hits <- stats |>
    html_elements(xpath = '//dd[@class="hits"]') |>
    html_text2() |>
    str_replace(fixed(","), "") |>
    as.numeric()



  work_info$published <- stats |>
    html_elements(xpath = '//dd[@class="published"]') |>
    html_text2() |>
    as.Date()

  if(work_info$chapters > 1){
    work_info$last_updated <- stats |>
      html_elements(xpath = '//dd[@class="status"]') |>
      html_text2() |>
      as.Date()
  } else {
    work_info$last_updated <- work_info$published
  }

  if("completed" %in% stats_elements){
    work_info$status <- "complete"
  } else {
    work_info$status <- "ongoing"
  }


  if("bookmarks" %in% stats_elements){
    work_info$bookmarks <- stats |>
      html_elements(xpath = '//dd[@class="bookmarks"]') |>
      html_text2() |>
      str_replace(fixed(","), "") |>
      as.numeric()
  } else {
    work_info$bookmarks <- 0
  }

  collections <- work |>
    html_elements(css = ".collections") |>
    html_children() |>
    html_attr("href")

  if(!identical(collections, character(0))) {
    work_info$collections <- paste(collections, collapse=", ")
  } else {
    work_info$collections <- NA
  }

  work_info$relationships <- work |>
    html_element(css = ".relationship .commas") |>
    html_text2() |>
    str_replace_all(fixed("\n"), ", ")

  work_info$warnings <- work |>
    html_element(css = ".warning .commas") |>
    html_text2() |>
    str_replace_all(fixed("\n"), ", ")

  work_info$age_category <- work |>
    html_element(css = ".rating .commas") |>
    html_text2()

  work_info$fandoms <- work |>
    html_element(css = ".fandom .commas") |>
    html_text2() |>
    str_replace_all(fixed("\n"), ", ")

  work_info$characters <- work |>
    html_element(css = ".character .commas") |>
    html_text2() |>
    str_replace_all(fixed("\n"), ", ")

  work_info$freeform_tags <- work |>
    html_element(css = ".freeform .commas") |>
    html_text2() |>
    str_replace_all(fixed("\n"), ", ")

  return(work_info)
}




GetMultipleWorkInfo <- function(workurl_df){

  author_works_info <- list()

  for (i in (1:nrow(workurl_df))) {
    link <- workurl_df$work_urls[i]
    author_works_info[[link]] <- GetWorkInfo(link)
  }
  author_works_info <- bind_rows(author_works_info, .id = "works_url")

  author_works_info
}
