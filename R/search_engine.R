.search_slug <- function(word = "AJAY D KAMDAR", quote = T, additional_terms = NULL) {
  if (quote){
    slug <- glue('"{word}"') %>% URLencode()
  } else {
    slug <- URLencode(word)
  }

  if (length(additional_terms) > 0) {
    a_slug <- URLencode(additional_terms)
    slug <- glue("{slug}+{a_slug}") %>% as.character()
  }
  as.character(slug)
}

.ddg <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://duckduckgo.com/?q="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}&ia=web") %>% as.character()
    url
  }

.bing <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://bing.com/search?q="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}") %>% as.character()
    url
  }

.yandex <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://yandex.ru/search/?text="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}&lr=213") %>% as.character()
    url
  }

.baidu <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://www.baidu.com/s?wd="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}&lr=213") %>% as.character()
    url
  }

.google <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://www.google.com/search?q="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}") %>% as.character()
    url
  }

.startpage <-
  function(word = "WENCUI ZHAO", quote = T, additional_terms = NULL) {
    base <- "https://ixquick.com/do/dsearch?query="
    slug <- .search_slug(word = word, quote = T, additional_terms = additional_terms)
    url <- glue("{base}{slug}") %>% as.character()
    url
  }


.ddg_urls <-
  function(words = c(
    "HUIPING MA",
    "DANA YI",
    "VEDANT SAMPATH",
    "YUWEI TU",
    "YUE YU",
    "YUTONG TAN",
    "WENSI FANG",
    "SHAOXIONG DING",
    "MAHESH VENKATAKRISHNAN",
    "SACHINDRANATH R SHETTY"
  ),
  quote = T,
  additional_terms = NULL,
  return_message = T) {
    all_data <-
      words %>%
      map_dfr(function(word){
        if (return_message) {
          glue("Building query for {word}") %>%
            as.character() %>%
            message()
        }

          urlDDG <- .ddg(word = word, quote = quote, additional_terms = additional_terms)
          tibble(termSearch = word, urlDDG)

      })

    all_data
  }

.google_urls <-
  function(words = c(
    "HUIPING MA",
    "DANA YI",
    "VEDANT SAMPATH",
    "YUWEI TU",
    "YUE YU",
    "YUTONG TAN",
    "WENSI FANG",
    "SHAOXIONG DING",
    "MAHESH VENKATAKRISHNAN",
    "SACHINDRANATH R SHETTY"
  ),
  quote = T,
  additional_terms = NULL,
  return_message = T) {
    all_data <-
      words %>%
      map_dfr(function(word){
        if (return_message) {
          glue("Building query for {word}") %>%
            as.character() %>%
            message()
        }
          urlGoogle <- .google(word = word, quote = quote, additional_terms = additional_terms)
          tibble(termSearch = word, urlGoogle)
      })

    all_data
  }

.startpage_urls <-
  function(words = c(
    "HUIPING MA",
    "DANA YI",
    "VEDANT SAMPATH",
    "YUWEI TU",
    "YUE YU",
    "YUTONG TAN",
    "WENSI FANG",
    "SHAOXIONG DING",
    "MAHESH VENKATAKRISHNAN",
    "SACHINDRANATH R SHETTY"
  ),
  quote = T,
  additional_terms = NULL,
  return_message = T) {
    all_data <-
      words %>%
      map_dfr(function(word){
        if (return_message) {
          glue("Building query for {word}") %>%
            as.character() %>%
            message()
        }
          urlStartPage <- .startpage(word = word, quote = quote, additional_terms = additional_terms)
          tibble(termSearch = word, urlStartPage)
      })

    all_data
  }

.yandex_urls <-
  function(words = c(
    "HUIPING MA",
    "DANA YI",
    "VEDANT SAMPATH",
    "YUWEI TU",
    "YUE YU",
    "YUTONG TAN",
    "WENSI FANG",
    "SHAOXIONG DING",
    "MAHESH VENKATAKRISHNAN",
    "SACHINDRANATH R SHETTY"
  ),
  quote = T,
  additional_terms = NULL,
  return_message = T) {
    all_data <-
      words %>%
      map_dfr(function(word){
        if (return_message) {
          glue("Building query for {word}") %>%
            as.character() %>%
            message()
        }
          urlYandex <- .yandex(word = word, quote = quote, additional_terms = additional_terms)
          tibble(termSearch = word, urlYandex)
      })

    all_data
  }


.baidu_urls <-
  function(words = c(
    "HUIPING MA",
    "DANA YI",
    "VEDANT SAMPATH",
    "YUWEI TU",
    "YUE YU",
    "YUTONG TAN",
    "WENSI FANG",
    "SHAOXIONG DING",
    "MAHESH VENKATAKRISHNAN",
    "SACHINDRANATH R SHETTY"
  ),
  quote = T,
  additional_terms = NULL,
  return_message = T) {
    all_data <-
      words %>%
      map_dfr(function(word){
        if (return_message) {
          glue("Building query for {word}") %>%
            as.character() %>%
            message()
        }
          urlBaidu <- .baidu(word = word, quote = quote, additional_terms = additional_terms)
          tibble(termSearch = word, urlBaidu)
      })

    all_data
  }

#' Query Search engies
#'
#' @param engines vector of search engines to search \itemize{
#' \item google
#' \item yandex
#' \item startpage
#' \item ddg
#' \item baidu
#' }
#' @param words
#' @param quote
#' @param rename_output
#' @param additional_terms
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
query_words <-
  function(engines = c("ddg", "google", "baidu", "startpage", "yandex"),
           words = NULL,
           quote = T,
           rename_output = NULL,
           additional_terms = NULL,
           return_message = T) {
    if (length(words) == 0) {
      stop("Enter Search Terms")
    }

    engine_slugs <- engines %>% str_to_lower()
    all_data <-
      engine_slugs %>%
      map(function(engine){
        if (engine == "google") {
          data <- .google_urls(words = words, quote = quote, additional_terms = additional_terms, return_message = return_message)
          return(data)
        }

        if (engine == "yandex") {
          data <- .yandex_urls(words = words, quote = quote, additional_terms = additional_terms, return_message = return_message)
          return(data)
        }

        if (engine == "ddg") {
          data <- .ddg_urls(words = words, quote = quote, additional_terms = additional_terms, return_message = return_message)
          return(data)
        }

        if (engine == "startpage") {
          data <- .startpage_urls(words = words, quote = quote, additional_terms = additional_terms, return_message = return_message)
          return(data)
        }

        if (engine == "baidu") {
          data <-
            .baidu_urls(words = words, quote = quote, additional_terms = additional_terms, return_message = return_message)
          return(data)
        }
      })

    all_data <- all_data %>% reduce(left_join, by = "termSearch")

    if (length(rename_output) > 0) {
      all_data <-
        all_data %>%
        rename(UQ(rename_output) := termSearch)
    }

    all_data
  }
