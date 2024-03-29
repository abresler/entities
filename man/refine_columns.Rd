% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refinr.R
\name{refine_columns}
\alias{refine_columns}
\title{Refine entity columns}
\usage{
refine_columns(
  data,
  entity_columns = NULL,
  use_business_suffix = T,
  phonics_methods = "soundex",
  phonics_length = 16L,
  use_n_gram_merge = T,
  edit_threshold = 1,
  ignore_words = NULL,
  numgram = 2,
  weight = c(d = 0.33, i = 0.33, s = 1, t = 0.5),
  snake_names = T,
  ...
)
}
\arguments{
\item{weight}{Numeric vector, indicating the weights to assign to the four edit operations (see details below), for the purpose of approximate string matching. Default values are c(d = 0.33, i = 0.33, s = 1, t = 0.5). This parameter gets passed along to the stringdist function. Must be either a numeric vector of length four, or NA.}

\item{snake_names}{}
}
\description{
Refine entity columns
}
\details{
The values of arg \code{weight} are edit distance values that
 get passed to the \code{stringdist} edit distance function. The
 param takes four arguments, each one is a specific type of edit, with
 default penalty value.
 \itemize{
 \item d: deletion, default value is 0.33
 \item i: insertion, default value is 0.33
 \item s: substitution, default value is 1
 \item t: transposition, default value is 0.5
 }
}
