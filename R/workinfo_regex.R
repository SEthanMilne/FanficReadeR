get_wordcount <- function(x){
  str_match(x, "Words\\:\\n(.*?)\\\n")[, 2]
}

get_language <- function(x){
  str_match(x, "Language\\:\\n(.*?)\\\n")[, 2]
}

get_chapters <- function(x){
  sub("/.*",
      "",
      str_match(x, "Chapters\\:\\n(.*?)\\\n")[, 2])
}

get_kudos <- function(x){
  str_match(x, "Kudos\\:\\n(.*?)\\\n")[, 2]
}

get_comments <- function(x){
  str_match(x, "Comments\\:\\n(.*?)\\\n")[, 2]
}

get_hits <- function(x){
  str_match(gsub("\n", " ", x), "Hits\\:(.*)")[, 2]
}

get_lastupdated <- function(x){
  str_match(x, "Updated\\:\\n(.*?)\\\n")[, 2]
}

get_completed <- function(x){
  str_match(x, "Completed\\:\\n(.*?)\\\n")[, 2]
}

get_status <- function(x) {
  if ("?" == sub(".*/",
                 "",
                 str_match(x, "Chapters\\:\\n(.*?)\\\n")[, 2])) {
    "Ongoing"
  } else {
    "Complete"
  }
}

get_bookmarks <- function(x){
  str_match(x, "Bookmarks\\:\\n(.*?)\\\n")[, 2]
}

get_relationships <- function(x){
  str_match(gsub("\n", " ", x), "Relationships\\:(.*?)Characters")[, 2]
}

get_warnings <- function(x){
  str_match(x, "Archive Warning\\:\\n(.*?)\\\n")[, 2]
}

get_agecategory <- function(x){
  str_match(x, "Rating\\:\\n(.*?)\\\n")[, 2]
}

get_fandoms <- function(x){
  str_match(gsub("\n", " ", x), "Fandoms\\:(.*?)Relationships")[, 2]
}
