library(tidyverse)
library(govtrackR)
library(textclean)
library(textshape)
setwd("~")
df_sbirs <- "Desktop/abresler.github.io/r_packages/govtrackR/data/all_sbir_sba.rda" %>% govtrackR::read_rda()
x <- df_sbirs %>%
  select(nameCompany) %>%
  filter(!is.na(nameCompany)) %>%
  pull()



# character classes -------------------------------------------------------

## Words
x %>% str_extract_all("\\w", simplify = F)

x %>% str_extract_all("[A-z0-9_]", simplify = F)
x %>% str_extract_all("[A-Za-z0-9_]", simplify = F)
x %>% str_extract_all("^[A-Za-z0-9_]", simplify = F)

## replace all word characters
x %>% str_replace_all("\\w","")

x %>% str_match_all("[[:alnum:]]")
x %>% str_extract_all("[[:alnum:]]")
x %>% str_remove_all()



# textclean ---------------------------------------------------------------

x %>% textclean::check_text()

x %>% str_split(" DBA ") %>% map_chr(function(x){
  x[[length(x)]]
})

x %>% textclean::replace_non_ascii()
