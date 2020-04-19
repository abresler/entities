#' Forebears country dictionary
#'
#' @return
#' @export
#'
#' @examples
#' dictionary_fb_countries()
dictionary_fb_countries <-
  memoise::memoise(function() {
    data <-
      "https://forebears.io/assets/geoJSON/454481.Nations.json" %>%
      fromJSON(simplifyDataFrame = T)

    ids <- data$features$id %>% as.numeric()

    data <-
      data$features$properties %>% as_tibble() %>%
      set_names(c("nameCountry", "codeCountry", "isDisputed")) %>%
      mutate(isDisputed = ifelse(is.na(isDisputed), F, T)) %>%
      mutate(idCountry = ids) %>%
      select(idCountry, everything())
    data
  })

.parse_region_json <-
  function(url = "https://forebears.io/assets/geoJSON/17092.Regions.json"){
    data <-
      url %>%
      fromJSON(simplifyDataFrame = T)
    parts <- url %>%str_split("/") %>% flatten_chr()
    idCountry <- parts[[length(parts)]] %>% str_split("\\.") %>% flatten_chr() %>%  .[[1]] %>% as.numeric()
    id <- data$features$id %>% as.numeric()
    data$features$geometry
    data <-
      data$features$properties %>% as_tibble()

    data %>%
      setNames(c("nameRegion")) %>%
      mutate(idRegion = id,
             idCountry) %>%
      select(idCountry, everything())

  }

dictionary_fb_regions <-
 function() {

 }

.format_data <-
  function(data){
    count_cols <-
      data %>% select(dplyr::matches("^count[A-Z]|^rank[A-Z]")) %>%
      select(-matches("^country[A-Z]")) %>%
      names()
    percent_cols <-
      data %>% select(dplyr::matches("pct|percent[A-Z]")) %>% names()

    if (length(count_cols) > 0) {
      data <-
        data %>%
        mutate_at(count_cols,
                  list(function(x) {
                    x %>% as.character() %>% readr::parse_number() %>% formattable::comma(digits = 0)
                  }))
    }

    if (length(percent_cols) > 0) {
      data <-
        data %>%
        mutate_at(percent_cols,
                  list(function(x) {
                    x %>% as.character() %>% readr::parse_number() %>% formattable::percent(digits = 8)
                  }))
    }


    data

  }

.generate_last_name_url <-
  function(last_name) {

    slug <-
      last_name %>%
      str_replace_all("\\ |\\-", "\\_") %>%
      str_remove_all("\\.") %>%
      URLencode() %>%
      str_replace_all("\\'", "%60") %>%
      str_to_lower()
    glue::glue("https://forebears.io/surnames/{slug}") %>% as.character()
  }

.fb_name_urls <-
  function(last_names) {
    last_names %>%
      map_dfr(function(last_name){
        url <- .generate_last_name_url(last_name = last_name)
        tibble(nameLast = last_name, urlForebearsName = url)
      })
  }

.parse_fb_name_url <- function(url = "https://forebears.io/a?c=Name&m=getGeo&i=209524&p=18000", return_wide = T) {
  options(scipen = 9999999)
  data <-
    url %>% fromJSON(simplifyDataFrame = T) %>% enframe() %>% mutate(value = value %>% map(enframe)) %>% unnest() %>% mutate(value = value %>% map(enframe)) %>%
    unnest() %>%
    mutate(value = value %>% flatten_dbl()) %>%
    setNames(c("year", "idCountry", "item", "value")) %>%
    mutate_at(c("year", "idCountry",  "value"),
              as.numeric) %>%
    mutate(urlForebearsAPI = url)

  if (return_wide) {
    data <- data %>% spread(item, value)
  }
  data

}

.world_rank <- function(page) {
  page %>% html_nodes(".statistic-number") %>% html_text() %>%
    parse_number()
}

.world_pop <- function(page) {
  page %>% html_nodes("b") %>% html_text() %>% .[[1]] %>% readr::parse_number()
}

.world_prevelence <-
  function(page) {
    values <-
      page %>% html_nodes(".detail") %>% html_text()
    items <- c("countryMostPrevalent", "countryHighestDensity")
    values <- values[1:length(items)]
    tibble(item = items, value = values) %>%
      spread(item, value) %>%
      select(one_of(items))
  }

.parse_meaning <-
  function(page) {
    page %>% html_nodes(".text-justify p") %>% html_text() %>% unique() %>% str_c(collapse = ", ") %>%
      stringi::stri_encode("UTF8")
  }

.parse_table_similar <-
  function(page) {
    data <-
      page %>% html_nodes(".container table") %>% html_table(fill = F) %>% .[[2]] %>% as_tibble() %>%
      set_names(c("nameLast", "rankSimilarity", "countIncidence", "remove")) %>%
      select(-remove) %>%
      mutate_at(c("rankSimilarity", "countIncidence"),
                list(function(x) {
                  x %>% as.character() %>% parse_number()
                }))
    data

  }

.parse_table_country <- function(page) {
  yearData <-
    page %>% html_nodes(".nav-tabs .active") %>% html_text() %>%
    .[[1]] %>% parse_number()
  data <-
    page %>% html_nodes(".container table") %>% html_table(fill = F) %>% .[[1]] %>% as_tibble() %>%
    set_names(c("nameCountry", "countPeople", "frequency", "rankNameCountry")) %>%
    mutate_at(c("countPeople", "rankNameCountry"), list(function(x){
      x %>% as.character() %>% parse_number()
    })) %>%
    separate(
      "frequency",
      into = c("frequency", "pctNameCountry"),
      sep = "\\:",
      convert = T
    ) %>%
    mutate(
      yearData,
      pctNameCountry = parse_number(pctNameCountry),
      pctNameCountry = frequency / pctNameCountry
    ) %>%
    select(-frequency) %>%
    select(yearData, everything())

  data
}




# urls --------------------------------------------------------------------
.parse_fb_surname_url <-
  function(url = "https://forebears.io/surnames/zhao", include_similar_names = F, return_message = T) {
    if (return_message) {
      ln <- url %>% str_remove_all("https://forebears.io/surnames/") %>% URLdecode() %>% str_to_upper()
      glue::glue("Acquiring data about last name {ln}") %>% message()
    }
  page <-
    read_html(x = url)
  .world_rank_safe <- possibly(.world_rank, NA_integer_)
  .world_pop_safe <- possibly(.world_pop, NA_integer_)
  .world_prevelence_safe <- possibly(.world_prevelence, tibble())
  .parse_meaning_safe <- possibly(.parse_meaning, NA_character_)
  .parse_table_country_safe <- possibly(.parse_table_country, tibble())
  .parse_table_similar_safe <- possibly(.parse_table_similar, tibble())
  world_rank <- .world_rank_safe(page = page)
  world_pop <- .world_pop_safe(page = page)
  df_prev <-
    .world_prevelence_safe(page = page) %>%
    asbmisc::remove_na_columns()
  meaning <- .parse_meaning_safe(page = page)
  df_countries <-
    .parse_table_country_safe(page = page) %>%
    asbmisc::remove_na_columns()

  if (include_similar_names) {
  df_similar <- .parse_table_similar_safe(page = page)
  }
  data <- tibble(rankNameWorld = world_rank, countPopulationNameWorld = world_pop, descriptionName = meaning,urlForebearsName = url)

  if (ncol(df_prev) > 0) {
    data <- data %>% bind_cols(df_prev)
  }

  if (nrow(df_countries) > 0) {
    data <- data %>%
      mutate(dataCountryPopularity = list(df_countries))
  }

  if (include_similar_names) {
    if (nrow(df_similar) > 0) {
      data <- data %>% mutate(dataSimilarNames = list(df_similar))
    }
  }
  data
}

.generate_country_url <-
  function(country = c("New Zealand")) {
    slug <- country %>% str_to_lower() %>% str_replace_all("\\ ", "\\-")
    url <-
      glue::glue("https://forebears.io/{slug}") %>% as.character()
    url
  }

.generate_country_urls <-
  function(countries = c("New Zealand")) {
    countries %>%
      map_dfr(function(country){
        url <- .generate_country_url(country = country)
        tibble(nameCountry = country, urlForebearsCountry = url)
      })
  }

.parse_country_url <-
  memoise::memoise(function(url = "https://forebears.io/taiwan", return_message = T) {
    if (return_message) {
      ln <- url %>%
        str_remove_all("https://forebears.io/") %>% URLdecode() %>% str_to_upper()
      glue::glue("Acquiring data about popular names in {ln}") %>% message()
    }
    page <- url %>% read_html()
    yearData <-
      page %>% html_nodes(".nav-tabs .active") %>% html_text() %>%
      .[[1]] %>% parse_number()
    data <-
      page %>% html_nodes(".container table") %>% html_table(fill = F) %>% .[[1]] %>% as_tibble() %>%
      set_names(c("rankName", "nameLast", "countPeople", "frequency")) %>%
      mutate_at(c("rankName", "countPeople"), list(function(x){
        x %>% as.character() %>% parse_number()
      })) %>%
      separate(
        "frequency",
        into = c("frequency", "pctNameCountry"),
        sep = "\\:",
        convert = T
      )

    data <-
      data %>%
      mutate(
        yearData,
        frequency = parse_number(as.character(frequency)),
        pctNameCountry = parse_number(as.character(pctNameCountry)),
        pctNameCountry = frequency / pctNameCountry
      ) %>%
      select(-frequency) %>%
      select(yearData, everything()) %>%
      mutate(urlForebearsCountry = url)

    data
  })

# functions ---------------------------------------------------------------

#' US political affiliations by last name
#'
#' @return
#' @export
#'
#' @examples
#' us_political_affiliation()
us_political_affiliation <-
  memoise::memoise(function(snake_names = F) {
    page <- "https://forebears.io/united-states/demographics/political-affiliation-surname" %>%
      read_html()
    table <- page %>% html_table(fill = F)

    df_reps <-
      table[[1]] %>% as_tibble() %>% set_names(c(
        "nameLast",
        "pctRegistered",
        "sizeSample",
        "rankOverall",
        "remove"
      )) %>%
      mutate(nameParty = "Republican") %>%
      mutate_at(c("pctRegistered", "sizeSample", "rankOverall"),
                list(function(x) {
                  x %>% parse_number()
                })) %>%
      mutate(pctRegistered = pctRegistered / 100)

    df_dem <-
      table[[2]] %>% as_tibble() %>% set_names(c(
        "nameLast",
        "pctRegistered",
        "sizeSample",
        "rankOverall",
        "remove"
      )) %>%
      mutate(nameParty = "Democrat") %>%
      mutate_at(c("pctRegistered", "sizeSample", "rankOverall"),
                list(function(x) {
                  x %>% parse_number()
                })) %>%
      mutate(pctRegistered = pctRegistered / 100)

    countryRank <- page %>% html_nodes("td:nth-child(5)") %>% map_chr(function(x){
      x %>% html_nodes("span") %>% html_attr("title") %>% .[[1]]
    })

    countryDensity <- page %>% html_nodes("td:nth-child(5)") %>% map_chr(function(x){
      x %>% html_nodes("span") %>% html_attr("title") %>% .[[2]]
    })

    data <-
      df_reps %>%
      bind_rows(df_dem) %>%
      select(-remove) %>%
      mutate(countryRank,
             countryDensity) %>%
      select(nameParty, everything()) %>%
      .format_data()

    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data

  })

#' Forebears US Religion data
#'
#' @return
#' @export
#'
#' @examples
#' us_religions()
us_religions <-
  memoise::memoise(function(snake_names = F) {
    page <-
      "https://forebears.io/united-states/demographics/religion-surname" %>%
      read_html()
    table <- page %>% html_table(fill = F)
    religions <- page %>% html_nodes("h3") %>% html_text()
    df_religions <-
      tibble(nameReligion = religions) %>%
      mutate(idRow = 1:n())

    data <-
      1:nrow(df_religions) %>%
      map_dfr(function(x){
        table[[x]] %>%
          as_tibble() %>%
          set_names(c(
            "nameLast",
            "pctAdherants",
            "sizeSample",
            "rankOverall",
            "remove"
          )) %>%
          mutate(nameReligion = df_religions$nameReligion[[x]]) %>%
          mutate_at(c("pctAdherants", "sizeSample", "rankOverall"),
                    list(function(x) {
                      x %>% as.character() %>% parse_number()
                    })) %>%
          mutate(pctAdherants = pctAdherants / 100) %>%
          select(nameReligion, everything()) %>%
          select(-remove)
      })

    countryRank <- page %>% html_nodes("td:nth-child(5)") %>% map_chr(function(x){
      x %>% html_nodes("span") %>% html_attr("title") %>% .[[1]]
    })

    countryDensity <- page %>% html_nodes("td:nth-child(5)") %>% map_chr(function(x){
      x %>% html_nodes("span") %>% html_attr("title") %>% .[[2]]
    })

    data <-
      data %>%
      mutate(countryRank, countryDensity) %>%
      .format_data()

    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data

  })

#' Forebears last name data
#'
#' @param last_names vector of last names
#' @param include_similar_names if \code{TRUE} includes table about similar names
#' @param return_message if \code{TRUE} returns message
#' @param nest_data  if \code{TRUE} nests tibble
#' @param assign_to_environment if \code{TRUE} assigns nested tibbles to environment
#' @param sleep_time if not \code{NULL} time between queiries
#'
#' @return
#' @export
#'
#' @examples
fb_last_names <-
  function(last_names = NULL,
           sleep_time = NULL,
           include_similar_names = F,
           return_message = T,
           nest_data = F,
           snake_names = F,
           assign_to_environment = T) {
    if (length(last_names) == 0) {
      stop("Enter last name")
    }
    if (include_similar_names) {
      include_similar_names <- T
    }
    .parse_fb_surname_url_safe <-
      possibly(.parse_fb_surname_url, tibble())

    df_urls <-
      .fb_name_urls(last_names = last_names)

    all_data <-
      df_urls$urlForebearsName %>%
      map_dfr(function(url){
        data <-
          .parse_fb_surname_url_safe(url = url, include_similar_names = include_similar_names, return_message = return_message)

        if (length(sleep_time) > 0) {
          Sys.sleep(time = sleep_time)
        }

        data
      })

    all_data <-
      all_data %>%
      left_join(df_urls, by = "urlForebearsName") %>%
      select(nameLast, everything())

    if (assign_to_environment) {
      data_names <-
        all_data %>% select(matches("data")) %>% names()
      data_names %>%
        walk(function(data_name){
          base_names <- all_data %>% select(-matches("data")) %>% names()
          glue::glue("Assigning {data_name} to environment") %>% message()
          table_name <- case_when(data_name == "dataCountryPopularity" ~ "df_popularity",
                    TRUE ~ "df_similar")
          df <-
            all_data %>%
            select(one_of(c(base_names, data_name))) %>%
            unnest() %>%
            .format_data()

          assign(x = table_name, value = df, envir = .GlobalEnv)
        })
    }

    if (!nest_data) {
      all_data <-
        all_data %>%
        unnest()
    }

    all_data  <-
    all_data %>%
      .format_data()

    if (snake_names) {
      all_data <-
        all_data %>%
        janitor::clean_names()
    }

    all_data
  }

#' Forebears country data
#'
#' @param locations vector of countries
#' @param return_message if \code{TRUE} returns message
#'
#' @return
#' @export
#'
#' @examples
#' fb_locations(locations = c("China"))
fb_locations <-
  function(locations = NULL, snake_names = F, return_message = T) {
    if (length(locations) == 0) {
      stop("Enter loctations")
    }
    df_locations <- .generate_country_urls(countries = locations)
    .parse_country_url_safe <- possibly(.parse_country_url, tibble())

    all_data <-
      df_locations$urlForebearsCountry %>%
      map_dfr(function(url){
        .parse_country_url(url = url, return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(df_locations, by = "urlForebearsCountry") %>%
      .format_data() %>%
      select(nameCountry, everything())

    if (snake_names) {
      all_data <-
        all_data %>%
        janitor::clean_names()
    }

    all_data
  }


download_fb_names <-
  function(last_names = c(),
         folder_path = "Desktop/data/forebears/surnames/data",
         sleep_time = 2) {
  oldwd <- getwd()
  setwd("~")
  .build_folder(path = folder_path)
  if (!dir.exists(folder_path)) {
    if (oldwd != getwd()) {
      setwd(oldwd)
    }
    return(invisible())
  }

  setwd(folder_path)
  fb_last_names_safe <- possibly(fb_last_names, tibble())
  finished <- list.files() %>% str_remove_all(".rda")

  df_ln <-
    tibble(last_name = last_names) %>%
    mutate(
      slug_ln =
        str_to_lower(last_name) %>%
        str_replace_all("\\ |\\-", "\\_") %>%
        str_remove_all("\\.") %>%
        str_replace_all("\\'", "%60") %>%
        str_to_lower()
    )

  df_finished <- tibble(slug_ln = finished, finished = T)

  missing_ln <-
    df_ln %>%
    left_join(
      df_finished, by = "slug_ln"
    ) %>%
    filter(is.na(finished)) %>%
    pull(slug_ln)

  missing <- df_ln %>%
    filter(slug_ln %in% missing_ln) %>%
    pull(last_name)


  missing %>%
    walk(function(last_name) {
      data <-
        fb_last_names_safe(last_names = last_name,
                           nest_data = T,
                           sleep_time = sleep_time)

      if (nrow(data) == 0) {
        return(invisible())
      }
      slug <-
        last_name %>% str_to_lower() %>% str_replace_all("\\ ", "\\_")
      file <- glue("{slug}.rda")
      glue("Saving {slug}") %>% message()
      data %>% save(file = file)
    })


}
