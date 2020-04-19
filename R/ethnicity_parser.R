.classify_last_name <-
  function(last_name = "Smith",
           return_message = T,
           include_probabilities = F) {
    library(wru)
    if (return_message) {
      glue("Classifying {last_name}") %>% message()
    }
    data <- tibble(surname = last_name)
    df <- wru::predict_race(data, surname.only = T)

    df <-
      as_tibble(df) %>% gather(race, prob, -surname) %>%
      mutate(
        race =  race %>% str_remove_all("pred."),
        race =   case_when(
          race == "asi" ~ "Asian",
          race == "bla" ~ "Black",
          race == "his" ~ "Hispanic",
          race == "whi" ~ "White",
          TRUE ~ "Other"
        )
      )
    prediction <-
      df %>% filter(prob == max(prob)) %>% pull(race) %>% .[[1]]

    if (return_message) {
      glue("{last_name} is likely {prediction}") %>% message()
    }

    data <-
      data %>% mutate(typeWRUPrediction = prediction)

    if (include_probabilities) {
      data <- data %>% mutate(dataRaceProbabilities = list(df))
    }

    data

  }

#' Classify last names
#'
#' Classify a vectore last names using the WRU model
#'
#' @param last_names vector of last names
#' @param include_probabilities if \code{TRUE} appends probabilities
#' @param last_name_column if not \code{NULL} last name column
#' @param return_message
#'
#' @return
#' @export
#' @import wru
#' @examples
#' classify_last_names(last_names = c("Jackson", "Cheng", "Wen"), include_probabilities = F)
classify_last_names <-
  function(last_names = c("Jackson", "Cheng", "Wen"),
           include_probabilities = F,
           last_name_column = "nameLast",
           snake_names = F,
           return_message = T) {
    .classify_last_name_safe <- possibly(.classify_last_name, tibble())

    all_data <-
      last_names %>%
      unique() %>%
      map_dfr(function(last_name) {
        .classify_last_name_safe(
          last_name = last_name,
          return_message = return_message,
          include_probabilities = include_probabilities
        )
      })

    if (length(last_name_column) > 0) {
      all_data <-
        all_data %>%
        rename(UQ(last_name_column) := surname)
    }

    if (snake_names) {
      all_data <- all_data %>%
        janitor::clean_names()
    }

    all_data
  }

.tbl_classify_last_names <-
  function(data,
           include_probabilities = F,
           last_name_column = NULL,
           return_message = T) {
    if (length(last_name_column) == 0) {
      stop("Please enter last name colum")

    }

    last_names <-
      data %>%
      select(!!last_name_column) %>%
      distinct() %>%
      pull() %>%
      discard(function(x) {
        is.na(x)
      })

    df <-
      classify_last_names(
        last_names = last_names,
        include_probabilities = include_probabilities,
        last_name_column = last_name_column
      )

    data <-
      data %>%
      left_join(df, by = last_name_column)

    data
  }

#' Parse name column into parts
#'
#' @param data
#' @param name_column
#' @param include_name_type
#' @param return_only_names
#'
#' @return
#' @export
#' @import humaniformat tibble rlang purrr stringr
#'
#' @examples
#' library(dplyr)
#' starwars %>% tbl_last_name(name_column = "name")
tbl_last_name <-
  function(data,
           name_column = "namePrincipalInvestigator",
           include_name_type = F,
           snake_names = F,
           return_only_names = F) {
    if (length(name_column) == 0) {
      stop("Enter name column")
    }

    all_names <- data %>% pull(name_column) %>% unique()

    df_names <-
      humaniformat::parse_names(names = all_names) %>%
      as_tibble() %>%
      setNames(c(
        "salutation",
        "nameFirst",
        "nameMiddle",
        "nameLast",
        "suffix",
        "nameFull"
      ))

    if (include_name_type) {
      slug <-
        name_column %>% str_remove_all("^name|^type|^slug|^description")


      names(df_names) <- names(df_names) %>% str_c(slug)
    }


    df_names <-
      df_names %>%
      mutate(UQ(name_column) := all_names) %>%
      select(one_of(name_column), everything())

    if (return_only_names) {
      return(df_names)
    }

    remove_cols <-
      names(data)[names(data) %in% names(df_names)] %>% str_remove_all(name_column) %>% discard(function(x) {
        x == ""
      })

    if (length(remove_cols) > 0) {
      data <- data %>%
        select(-one_of(remove_cols))
    }

    data <- data %>% left_join(df_names, by = name_column)

    if (snake_names) {
      data <- data %>%
        janitor::clean_names()
    }

    data
  }

.classify_names <-
  function(data,
           name_column = NULL,
           include_probabilities = F,
           include_name_type = T,
           return_message = T) {
    ### deal with entities

    has_entity_column <-
      names(data) %>% endsWith("Entity") %>% sum(na.rm = T) > 0


    if (has_entity_column) {
      entity_col <- names(data)[names(data) %>% endsWith("Entity")]

      data <- data %>%
        mutate(idRow = 1:n())

      df_people <-
        data %>%
        filter(!(!!sym(entity_col)))
      if (nrow(df_people) == 0) {
        data <-
          data %>%
          mutate(typeWRUPrediction = "Entity") %>%
          select(-idRow)

        if (include_name_type) {
          part <-
            name_column %>% str_remove_all("^name|^type|^description")
          new_name <- str_c("typeWRUPrediction", part)
          data <- data %>%
            rename(UQ(new_name) := typeWRUPrediction)
        }
        return(data)
      }

      df_people <-
        tbl_last_name(
          data = df_people,
          name_column = name_column,
          include_name_type = T,
          return_only_names = F
        )

      df_people <-
        .tbl_classify_last_names(
          data = df_people,
          last_name_column = df_people %>% select(matches("nameLast")) %>% names(),
          include_probabilities = include_probabilities,
          return_message = return_message
        )

      data <-
        data %>%
        filter((!!sym(entity_col))) %>%
        mutate(typeWRUPrediction = "Entity") %>%
        bind_rows(df_people) %>%
        arrange(idRow) %>%
        select(-idRow)

      if (include_name_type) {
        part <-
          name_column %>% str_remove_all("^name|^type|^description")
        new_name <- str_c("typeWRUPrediction", part)
        data <- data %>%
          rename(UQ(new_name) := typeWRUPrediction)
      }

      return(data)
    }

    data <-
      tbl_last_name(
        data = data,
        name_column = name_column,
        include_name_type = T,
        return_only_names = F
      )

    all_data <-
      .tbl_classify_last_names(
        data = data,
        last_name_column = data %>% select(matches("nameLast")) %>% names(),
        include_probabilities = include_probabilities,
        return_message = return_message
      )

    all_data <- all_data %>%
      mutate(typeWRUPrediction = case_when(is.na(typeWRUPrediction) ~ "Other",
                                           TRUE ~ typeWRUPrediction))

    if (include_name_type) {
      part <-
        name_column %>% str_remove_all("^name|^type|^description")
      new_name <- str_c("typeWRUPrediction", part)
      all_data <- all_data %>%
        rename(UQ(new_name) := typeWRUPrediction)
    }

    all_data
  }


#' Classify name columns for WRU ethnicity
#'
#' @param data
#' @param name_columns
#' @param include_probabilities
#' @param include_name_type
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
classify_wru_names <-
  function(data,
           name_columns = NULL,
           snake_names = F,
           include_probabilities = F,
           include_name_type = T,
           return_message = T) {
    if (length(name_columns) == 0) {
      "Enter name columns to classify"
      return(data)
    }
    .classify_names_safe <- possibly(.classify_names, tibble())
    name_columns %>%
      walk(function(name_column) {
        data <<- .classify_names_safe(
          data = data,
          name_column = name_column,
          include_probabilities = include_probabilities,
          return_message = return_message,
          include_name_type = include_name_type
        )
        data
      })

    if (snake_names) {
      data <- data %>%
        janitor::clean_names()
    }
    data
  }
