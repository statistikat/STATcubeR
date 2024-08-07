% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/od_list.R
\name{od_list}
\alias{od_list}
\title{List available Opendata datasets}
\usage{
od_list(unique = TRUE, server = c("ext", "red"))
}
\arguments{
\item{unique}{some datasets are published under multiple groups.
They will only be listed once with the first group they appear in unless
this parameter is set to \code{FALSE}.}

\item{server}{the open data server to use. Either \code{ext} for the external
server (the default) or \code{red} for the editing server. The editing server
is only accessible for employees of Statistics Austria}
}
\value{
a \code{data.frame} with two columns
\itemize{
\item \code{"category"}: Grouping under which a dataset is listed
\item \code{"id"}: Name of the dataset which can later be used in
\code{\link[=od_table]{od_table()}}
\item \code{"label"}: Description of the dataset
}
}
\description{
\code{\link[=od_list]{od_list()}} returns a \code{data.frame } containing all datasets published at
\href{https://data.statistik.gv.at}{data.statistik.gv.at}
}
\examples{
df <- od_list()
df
subset(df, category == "Bildung und Forschung")
# use an id to load a dataset
od_table("OGD_fhsstud_ext_FHS_S_1")
}
