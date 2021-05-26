GetComments <-
  function (input,
            keep.text = TRUE,
            excl.author = FALSE) {
    ### Placeholder DF to store results
    comment_data <- matrix(ncol = 4, nrow = 1) %>%
      data.frame() %>%
      rename(
        user = X1,
        chapter = X2,
        date = X3,
        text = X4
      )


    for (i in 1:100) {
      ### loops through each comment page
      url <-
        paste0(
          WorkURL(input),
          "?page=",
          i,
          "&show_comments=true&view_adult=true&view_full_work=true#comments"
        )

      ### Gets HTML page
      comm <- GET(
        url,
        user_agent(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
        )
      ) %>%
        read_html()

      ### Downloads comments
      test_comm <-  comm %>%
        html_elements(css = "ol li") %>%
        html_text2() %>%
        data.frame()

      names(test_comm) <- "comments"

      ### Checks if comments are there - if not, breaks the loop
      if (0 ==
          test_comm %>%
          filter(grepl("on Chapter", comments)) %>%
          nrow()) {
        break
      }
      else{
        ### Get Comments
        comments <- test_comm

        names(comments) <- "comments"

        ### Extracts user, chapter, date, and text of comment
        comments <- comments %>%
          filter(grepl("on Chapter", comments)) %>%
          mutate(user = gsub(" ", "", str_match(comments, "(.*)on Chapter")[, 2])) %>%
          mutate(chapter = str_match(comments, "on Chapter +(.*?) +")[, 2]) %>%
          mutate(date = str_match(comments, paste0("on Chapter ", chapter,  " +(.*?)\\\n"))[, 2]) %>%
          mutate(text = str_match(
            gsub("\n", " ", comments),
            paste0(date, "(.*?)Comment Actions")
          )[, 2]) %>%
          select(-comments) %>%
          distinct(text, .keep_all = TRUE) ### Removes duplicates


        comment_data <- rbind(comment_data, comments)

        ### Sys.sleep command so that AO3 isn't overloaded
        Sys.sleep(2)
      }
    }



    ### Get Authors Name
    name <- comm %>%
      html_elements(css = ".heading") %>%
      html_text2() %>%
      data.frame()

    name <- name[6,]

    ### Flag comments made by the author
    comment_data <- comment_data[-1, ] %>%
      mutate(author = ifelse(user == name, 1, 0))

    ### Adds option to ignore author responses
    if (excl.author == TRUE) {
      comment_data <- comment_data %>%
        filter(author == 0) %>%
        select(-author)
    }

    ### Comments eat up space, so if you're interested in counts, then this
    ### parameter lets you not store the text of comments

    if (keep.text == FALSE) {
      comment_data %>%
        select(-text)

    } else{
      comment_data

    }


  }
