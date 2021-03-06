% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_get.R
\name{getResultPages}
\alias{getResultPages}
\title{Get query results.}
\usage{
getResultPages(mediaName, searchTerm)
}
\arguments{
\item{mediaName}{Name of digital media. Currently only Spiegel-Online, FAZ.net and Welt.de are supported.}

\item{searchTerm}{The keyword the search engine should queried for.}
}
\value{
The function returns all result-pages of the queried website as html on harddisk in the project-subfolder.
}
\description{
Queries the search engine of a digital media site for a specific searchterm.
}

