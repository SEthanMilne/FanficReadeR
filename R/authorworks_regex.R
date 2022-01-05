get_workstable <- function(x){
  sub(".*.Listing Works",
      "",
      gsub(
        "",
        "",
        x |>
          html_element(css = "#main") |>
          html_text2()
      )) |>
    str_match_all("\\\n(.*?)by do not cache") |>
    data.frame() |>
    select(X2) |>
    rowid_to_column() |>
    rename(Titles = X2, Work = rowid)
}

get_hits_alt <- function(x){
  str_match(x, "Hits\\:\\n(.*?)\\\n")[1, 2]
}

get_lastupdated_alt <- function(x){
  str_match(x, "\\n(.*?)\\\n\\nTags")[1, 2]
}

get_status_alt <- function(x) {
  str_match(x,paste0("\\n(.*?)\\\n\\n",
                     get_lastupdated_alt(x)))[1, 2]
}

get_relationships_alt <- function(x) {
  str_match(x,paste0("\\n(.*?)\\\n",
                     get_status_alt(x)))[1, 2]
}

get_warnings_alt <- function(x){
  str_match(x,paste0("\\n(.*?)\\\n",
                     get_relationships_alt(x)))[1, 2]
}

get_category_alt <- function(x){
  str_match(x, paste0("\\n(.*?)\\\n",
                      get_warnings_alt(x)))[1, 2]
}

get_fandoms_alt <- function(x) {
  str_match(x,paste0("\\nFandoms\\:(.*?)\\\n",
                     get_category_alt(x)))[1, 2]
}

check_if_noworks <- function(x){
  0 == sub(".*Works *(.*?) *Series.*", "\\1",
           gsub(
             "\n",
             " ",
             gsub(
               "[[:punct:]]",
               "",
               x %>%
                 html_element(css = "#dashboard") %>%
                 html_text2()
             )
           ))
}
