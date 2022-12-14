% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strata.R
\name{get_concept_strata}
\alias{get_concept_strata}
\title{Function that creates a date strata}
\usage{
get_concept_strata(
  connectionDetails,
  cdmDatabaseSchema,
  resultsDatabaseSchema,
  vocabularyDatabaseSchema,
  cohortTable,
  targetCohortId,
  name,
  domain,
  element,
  concept
)
}
\arguments{
\item{connectionDetails}{the connection Details object to connect to the database}

\item{cdmDatabaseSchema}{the schema that holds the cdm}

\item{resultsDatabaseSchema}{the results schema holding the cohort table}

\item{cohortTable}{the cohort table holding the cohort}

\item{targetCohortId}{the cohort Id used for the strata}

\item{name}{the name of the strata}

\item{domain}{the cdm table to search for a concept}

\item{element}{the element in a table to search for a concept}

\item{concept}{the concept Id to search in the cdm}
}
\value{
a dataframe representing the strata for the cohort
}
\description{
Function that creates a date strata
}
