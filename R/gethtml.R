# Downloads HTML page
get_html <- function(input){

  Sys.sleep(5.5)

  GET(
    input,
    user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"
    )
  ) |>
    read_html()

}
