\name{IDgch}
\alias{IDgch}
\title{Retrieves IDs of granchildren of ego}
\description{Retrieves IDs of grandchildren of vector of egos}
\usage{
IDgch(id, dataLH, keep_ego = FALSE)
}
\arguments{
  \item{id}{vector of IDs of egos}
  \item{dataLH}{Database. If missing, datac=datab}
  \item{keep_ego}{Option to show ID of ego together with ID of mother}
}
\details{IDgch uses IDch}
\value{ID of grandchildren or (if keep_ego=TRUE, data frame with ID of members of multiple generations). If ego has no grandchildren or IDs of grandchildren are not included in database, numeric(0) is returned and the message "No (grand)children of ego in database". If keep_ego=TRUE, an data frame is returned with the following columns: IDego, ID of ego's child that is mother or father of grandchildren, IDs of mother and father of grandchildren, ID of grandchildren, character sequence denoting the sex of grandparent, parent and sex of child.}
\author{Frans Willekens}
\examples{
data(dataLH)
# Grandchildren of ego with ID 1
IDgch(1,dataLH)
# Grandchildren of member of first generation
IDgch(sample(dataLH$ID[dataLH$gen==1],3),dataLH,keep_ego=TRUE)
}
