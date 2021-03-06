% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_get.R
\name{getConceptData}
\alias{getConceptData}
\title{Query DBpedia.}
\usage{
getConceptData(concept = "index", subjectLabel = NULL,
  spqlEndPoint = "http://de.dbpedia.org/sparql")
}
\arguments{
\item{concept}{name of the Wikipedia concept where to look after corresponding subjects.}

\item{subjectLabel}{if specified, additionally include matching subjects.}

\item{spqlEndPoint}{the sparcle endpoint where the query is send to.}
}
\value{
The function returns all subjects from DBpedia which are associated to the given concept as a data frame.
}
\description{
Queries the DBpedia using a SPARQL endpoint.
}

