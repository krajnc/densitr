% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{combine_footers}
\alias{combine_footers}
\title{Combines footer data from a dp object list into a single data
frame}
\usage{
combine_footers(dp.list)
}
\arguments{
\item{dp.list}{A list of dp objects, either from loading several
files using dpload or combined manually. Note: the list should
include only dp objects!}
}
\value{
A data frame, combining all footer data from dp.list
}
\description{
Given a dp object list, this function will extract all footers
(the additional measurement data) from all dp objects in a given
list and combine them in a single data frame. Will not work if
trying to combine footer from newer and older format of data.
}
\examples{
## load all files in directory
dp.list <- dpload(dp.directory = system.file("extdata", package = "densitr"))
combine_footers(dp.list)
}
\seealso{
dpload, combine_data.
}
