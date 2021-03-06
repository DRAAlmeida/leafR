% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{pointsByZSlice}
\alias{pointsByZSlice}
\title{Count number of points in each Z slice}
\usage{
pointsByZSlice(Z, maxZ)
}
\arguments{
\item{Z}{numeric vector. The heights vector.}

\item{maxZ}{numeric. The maximum height expected in the whole dataset.}
}
\value{
A \code{\link[base:list]{list}} of point counts in each Z slice of 1 meter
}
\description{
Count number of points in each Z slice
}
