\name{IDch}
\alias{IDch}
\title{Retrieves ID of children of ego}
\description{Retrieves ID of children of ego or children of vector of egos}
\usage{
IDch(id, dataLH, keep_ego = FALSE)
}
\arguments{
  \item{id}{ID of ego(s)}
  \item{dataLH}{Database. }
  \item{keep_ego}{Option to link show ID of ego together with ID of mother }
}
\value{ID of children or (if keep_ego=TRUE, data frame with ID of ego and ID of children). If ego has no children or IDs of children are not included in database, numeric(0) is returned and the message "No (grand)children of ego in database". If keep_ego=TRUE, an data frame is returned with the following columns: IDego, ID of mother of children, ID of father of children, ID of children, character sequence denoting the sex of parent and sex of child.}
\author{Frans Willekens }
\examples{
data(dataLH)
IDch(1,dataLH)
IDch(sample (dataLH$ID,10),dataLH,keep_ego=TRUE)
}