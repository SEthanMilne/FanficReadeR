WorkURL <- function(x){

  if (grepl("\\/chapters\\/", x )) {
    gsub(
      "\\/chapters\\/[0-9]*", "", x
    )
  } else {
    x
  }
}
