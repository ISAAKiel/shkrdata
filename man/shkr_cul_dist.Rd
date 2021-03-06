% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shkr.R
\name{shkr_cul_dist}
\alias{shkr_cul_dist}
\title{\code{shkr_cul_dist} calculates distance matrix}
\usage{
shkr_cul_dist(ts)
}
\arguments{
\item{ts}{spectrum of types as produced by \code{shkr_ts}}
}
\value{
distance matrix
}
\description{
\code{shkr_cul_dist} calculates distance matrix
}
\examples{
dm <- shkr_cul_dist(ts)
}
\author{
Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>

shkr_cul_dist is a wrapper of moin::dist_matr with method fixed to "euclidean"
}
