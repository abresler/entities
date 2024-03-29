% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_text}
\alias{clean_text}
\title{Clean Text}
\usage{
clean_text(
  x,
  case = NULL,
  split_characters = NULL,
  custom_regex = NULL,
  remove_bracketed_text = T,
  remove_digits = F,
  remove_commas = F,
  remove_periods = F,
  remove_html = T,
  remove_dash = F,
  remove_kern = F,
  remove_symbols = F,
  fix_comma_space = T,
  remove_ampersand = F,
  remove_entity_types = F,
  keep_entity_type = c("Association"),
  ...
)
}
\arguments{
\item{custom_regex}{}
}
\description{
Clean Text
}
