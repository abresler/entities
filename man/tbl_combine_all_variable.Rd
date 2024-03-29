% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzyjoins.R
\name{tbl_combine_all_variable}
\alias{tbl_combine_all_variable}
\title{Create combonation of a variable}
\usage{
tbl_combine_all_variable(data, variable, override_names = F)
}
\arguments{
\item{data}{a `tibble`}

\item{variable}{variable name}

\item{override_names}{if `TRUE` overrides `from` `to` with actual variable name}
}
\value{
`tibble`
}
\description{
Create combonation of a variable
}
\examples{
library(entities)
tbl_combine_all_variable(data = ggplot2::diamonds, variable = 'color')


}
