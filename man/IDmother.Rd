\name{IDmother}
\alias{IDmother}
\title{Retrieves ID of mother of ego}
\description{Retrieves the ID of mother of ego or mothers of vector of egos }
\usage{
IDmother(id, dataLH, keep_ego=FALSE)
}
\arguments{
  \item{id}{ID}
  \item{dataLH}{Datbase. If missing, datc = dataLH}
  \item{keep_ego}{Option to show ID of ego together with ID of mother}
}
\value{ ID of mother or (if keep_ego=TRUE, object with ID of ego and ID of mother). Returns NA if ID of mother is not included in the database}
\author{ Frans Willekens}
\examples{
data(dataLH)
IDmother (sample (dataLH$ID,10),dataLH)
IDmother(sample (dataLH$ID,10),dataLH,keep_ego=TRUE)
}