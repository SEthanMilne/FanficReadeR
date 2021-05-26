GetChapterIndex <- function(input){
  index <- GET(
    paste0(input, "/navigate"),
    user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    )
  ) %>%
    read_html()

  ## Get Chapter names, numbers, and dates

  chapter_info <- index %>%
    html_element(css = "#main") %>%
    html_text2() %>%
    strsplits("\n") %>%
    data.frame()

  colnames(chapter_info) <- "title"

  end <- nrow(chapter_info) - 1
  chapter_info <- chapter_info[2:end,1] %>% data.frame()

  colnames(chapter_info) <- "title"

  chapter_info <- titles %>%
    mutate(title = paste0("START", title, "END")) %>% ### adding markers for start/end
    mutate(
      chapter = str_match(title, "START(.*?)\\.")[, 2],
      date = str_match(title, "\\((.*?)\\)END")[, 2]
    ) ### extracting chapter number and date


  for (i in 1:nrow(chapter_info)){
    chapter_info[i,1] <- gsub(paste0("START", chapter_info[i,2], "."), "", chapter_info[i,1])
    chapter_info[i,1] <- gsub(paste0("\\(", chapter_info[i,3], "\\)END"), "", chapter_info[i,1])
  }

  ### Get Chapter URLS

  links <- index %>%
    html_elements(css = "#main ol a") %>%
    html_attr("href") %>%
    data.frame()

  names(links) <- "url"

  links <- links %>%
    mutate(url = paste0("https://archiveofourown.org", url))

  chapter_info <- cbind(chapter_info, links)

  chapter_info

}
