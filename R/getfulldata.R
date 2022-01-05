GetFullData <- function(fandom, pages){

  output_list <- list()

  works_index <- GetFandomIndex(fandom, pages)

  for (i in 1:nrow(works_index)){
    temp_work <- works_index[i,]
    temp_name <- get_authorname(temp_work)
    temp_authorinfo <- GetAuthorInfo(temp_name)
    #temp_authorworks <- GetAuthorWorks(temp_name)
    #print("author works gotten")
    temp_workinfo <- GetWorkInfo(temp_work)
    temp_chapterinfo <- GetChapterInfo(temp_work)
    temp_comments <- GetComments(temp_work)

    temp_list <- list(
      work = temp_work,
      name = temp_name,
      authorinfo = temp_authorinfo,
      #authorworks = temp_authorworks,
      workinfo = temp_workinfo,
      chapterinfo = temp_chapterinfo,
      comments = temp_comments
    )

    output_list[[i]] <- temp_list

    Sys.sleep(60)

  }

  output_list
}
