GetAuthorAll <- function(input){
  list(GetAuthorInfo(input),
       GetAuthorWorks(input),
       GetAuthorBookmarks(input))
}
