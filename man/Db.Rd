\name{Db}
\alias{Db}
\title{Retrieves the date(s) of birth in decimal format}
\description{Retrieves the date(s) of birth from the database}
\usage{ Db(id, dataLH)}
\arguments{
  \item{id}{vector of IDs of egos}
  \item{dataLH}{Name of database. If absent, the name 'dataLH' is used.}
}
\value{Returns the dates of birth}
\author{Frans Willekens}
\examples{
# Date of birth of first individual in database
data(dataLH)
Db(1,dataLH) 
}
