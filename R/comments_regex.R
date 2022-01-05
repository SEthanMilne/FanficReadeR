comments_getuser <- function(x) {
  gsub(" ", "", str_match(x, "(.*)on Chapter")[, 2])
}

comments_getchapter <- function(x) {
  str_match(x, "on Chapter +(.*?) +")[, 2]
}

comments_getdate <- function(x, y) {
  str_match(x, paste0("on Chapter ", y,  " +(.*?)\\\n"))[, 2]
}

comments_gettext <- function(x, y) {
  str_match(gsub("\n", " ", x),
            paste0(y, "(.*?)Comment Actions"))[, 2]
}
