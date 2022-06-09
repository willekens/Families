\name{IDpartner}
\alias{IDpartner}
\title{Retrieves ID of partner of ego or allocate partner to ego}
\description{Retrieves ID of partners of vector of egos or randomly allocates partners to egos }
\usage{
IDpartner(idego = NULL,dataLH)
}
\arguments{
  \item{idego}{vector of ID of egos. If idego is missing, then the function allocates partners (from opposite sex) to egos. The allocation is random.}
  \item{dataLH}{Database. If missing, database 'datap' is used.}
}
\value{IDs of partners. If the argument idego is missing, then a data frame similar to 'dataLH' is returned with IDs of partners completed.
}
\author{Frans Willekens}
\examples{
data(dataLH)
IDpartner(idego=1,dataLH)
# Allocate partner to egos with ID 4,9,30.
IDpartner(idego=dataLH$ID[c(4,9,30)],dataLH)  
}