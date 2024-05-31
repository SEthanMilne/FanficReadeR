GPT_fanfic_summarize <- function(answer_my_question, my__gpt_API_key) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_gpt_API_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-0125",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = paste("Please write a 100-word summary of the following fanfiction chapter. Here is the text: ",answer_my_question)
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

GPT_fanfic_commenter <- function(answer_my_question, my_gpt_API_key) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_gpt_API_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "ft:gpt-3.5-turbo-0125:personal:selectedfanfics:8wyEQ7AS",
      temperature = 1,
      messages = list(
        list(
          role = "user",
          content = paste("Generate a kind comment for the following fanfiction excerpt in the style of an ordinary fanfiction user. Here is the excerpt: ",answer_my_question)
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}
