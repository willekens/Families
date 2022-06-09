\name{Dd}
\alias{Dd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieves the date(s) of death in decimal format}
\description{Retrieves the date(s) of death from the database}
\usage{
Dd(id, dataLH)
}
\arguments{
  \item{id}{vector of IDs of egos}
  \item{dataLH}{Name of database. If absent, the name 'dataLH' is used}
}
\value{Returns the date of death}
\author{Frans Willekens}
\examples{
# Date of death of first individual in database
data(dataLH)
Dd(1,dataLH) 
}
