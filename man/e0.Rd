\name{e0}
\alias{e0}
\title{Mean ages at death and probabilities of surviving to selected ages, by sex}
\description{Computes (a) Life expectancy at birth, (b) Probability of surviving at age 65, and (c) Probability of surviving at age 85}
\usage{
e0(dataLH)
}
\arguments{
  \item{dataLH}{The name of the database. If omitted, the name 'dataLH' is used.}
  }
\value{
\item{e0}{Mean ages at death}
\item{Prob65}{Probability of surviving at age 65}
\item{Prob85}{Probability of surviving at age 85}
}
\author{Frans Willekens}
\examples{
data(dataLH)
e0(dataLH)
}
