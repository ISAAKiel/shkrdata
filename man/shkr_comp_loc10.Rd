% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shkr.R
\name{shkr_comp_loc10}
\alias{shkr_comp_loc10}
\title{\code{shkr_comp_loc10} completes information in loc10}
\usage{
shkr_comp_loc10(loc10sel, data)
}
\arguments{
\item{loc10sel}{artefact tibble as produced by \code{shkr_filter_loc10}}

\item{data}{list of shkr tibbles as produced by \code{load_shkr2010}}
}
\value{
a tibble based on but containing information from all
other loc levels in data in one object
}
\description{
The function \code{shkr_comp_loc10} transferes information from
loc_01-loc_09 to loc_10 to make them directly available at loc_10
level
}
\examples{
shkr_fibulae_comp <- shkr_comp_loc10(shkr_fibulae, shkr)
}
\author{
Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
}
