\name{IDfather}
\alias{IDfather}
\title{Retrieves ID of father of ego}
\description{ Function to retrieve the ID of father of ego or fathers of vector of egos }
\usage{
IDfather(id, dataLH,keep_ego=FALSE)
}
\arguments{
  \item{id}{ID}
  \item{dataLH}{Datbase. If missing, datc = dataLH}
  \item{keep_ego}{Option to link show ID of ego togehter with ID of father}
}
\value{ ID of father or (if keep_ego=TRUE, object with ID of ego and ID of father). Returns NA if ID of father is not included in the database}
\author{ Frans Willekens}
\examples{
data(dataLH)
IDfather (sample (dataLH$ID,10),dataLH)
}