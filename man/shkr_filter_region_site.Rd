% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shkr.R
\name{shkr_filter_region_site}
\alias{shkr_filter_region_site}
\title{\code{shkr_filter_region_site} filter a shkr tibble/dataframe to a certain spatial extent}
\usage{
shkr_filter_region_site(data, sites, ftype = "loc01", a = 11,
  x_range = c(0, 3597713), y_range = c(0, 5664752))
}
\arguments{
\item{data}{shkr tibble as produced by \code{load_shkr2010}}

\item{sites}{shkr tibble as produced by \code{shkr_filter_loc06}}

\item{ftype}{filter type: "coords", "loc01", "loc02", "loc03", "loc04", "loc05", "loc06"}

\item{a}{integer loc_xx_id number}

\item{x_range}{vector: c(minimal, maximal)}

\item{y_range}{vector: c(minimal, maximal)}
}
\value{
a tibble as used as input but restricted to a certain area
}
\description{
\code{shkr_filter_region_site} filter a shkr tibble/dataframe to a certain spatial extent
}
\examples{
type <- 4
shkr_grave <- shkr_filter_loc06(shkr, type)
shkr_grave <- shkr_filter_region_site(shkr, shkr_grave, ftype = "loc03", a=81)
}
\author{
Franziska Faupel <\email{ffaupel@ufg.uni-kiel.de}>
}
