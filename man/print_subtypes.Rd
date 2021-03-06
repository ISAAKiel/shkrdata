% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shkr.R
\name{print_subtypes}
\alias{print_subtypes}
\title{\code{print_subtypes} prints subtypes of types}
\usage{
print_subtypes(data, typestring = "B", descr = FALSE)
}
\arguments{
\item{data}{shkr database as loaded by \code{load_shkr2010}}

\item{descr}{logical value, if TRUE the type-string is searched in the type description and not in the type code}

\item{type-string}{type code such as "B31" or "A3" or "B31.1". The dot "." is used as wild-card representing one digit.}
}
\value{
tibble of subtypes
}
\description{
The function print_subtypes prints and returns the subtypes of a type represented by the type-string containing a type code such as "B31". With the "descr" category it is also possible to submit a text-string, for which is searched in the type description.
}
\examples{
print_subtypes(shkr, typestring = "B311.21", descr = FALSE)
print_subtypes(shkr, typestring = "Ompha", descr = TRUE)
}
\author{
Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
}
