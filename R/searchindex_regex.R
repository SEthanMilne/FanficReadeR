get_searchURL <- function(input, date_from="", date_to="", complete="") {
  paste0( #note: always excludes explicit fics, per REB requirements
    "https://archiveofourown.org/works?work_search%5Bsort_column%5D=revised_at&work_search%5Bother_tag_names%5D=&exclude_work_search%5Brating_ids%5D%5B%5D=13&work_search%5Bexcluded_tag_names%5D=&work_search%5Bcrossover%5D=&work_search%5Bcomplete%5D=",
    complete, #T for complete fics only, F for incomplete fics only
    "&work_search%5Bwords_from%5D=&work_search%5Bwords_to%5D=&work_search%5Bdate_from%5D=",
    date_from, #YYYY-MM-DD
    "&work_search%5Bdate_to%5D=" ,
    date_to, #YYYY-MM-DD
    "&work_search%5Bquery%5D=&work_search%5Blanguage_id%5D=&commit=Sort+and+Filter&tag_id=",
    str_replace_all(
      str_replace_all(input, " ", "+"), "\\.", "*d*")
  )
}

get_searchURLpage <- function(input, page, date_from="", date_to="", complete="") {
  paste0(
    "https://archiveofourown.org/tags/",

    str_replace_all(str_replace_all(input, " ", "%20"), "\\.", "*d*"),
    "/works?commit=Sort+and+Filter&exclude_work_search%5Brating_ids%5D%5B%5D=13&page=",
    page,
    "&work_search%5Bcomplete%5D=",
    complete,
    "&work_search%5Bcrossover%5D=&work_search%5Bdate_from%5D=",
    date_from,

    "&work_search%5Bdate_to%5D=",
    date_to,

    "&work_search%5Bexcluded_tag_names%5D=&work_search%5Blanguage_id%5D=&work_search%5Bother_tag_names%5D=&work_search%5Bquery%5D=&work_search%5Bsort_column%5D=revised_at&work_search%5Bwords_from%5D=&work_search%5Bwords_to%5D="
  )
}
