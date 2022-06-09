\name{Multistate}
\alias{Multistate}
\title{Computes fertility table by birth order (multistate life table)}
\description{Computes multistate life table from female death rates and birth-order specific fertility rates
}
\usage{
Multistate(rates, mortality = 1)
}
\arguments{
  \item{rates}{Object with age- and sex-specific death rates (ASDR) and age- and birth-order specific fertility rates (ASFR). The object is created to Getrates_refyear.}
  \item{mortality}{If mortality=1, mortality included (default). If mortality=0, mortality disregarded}
}
\value{A list of two objects: (a) the multistate survival function (S) and multistate transition probabilities (P), and (b) other measures of the multistate life table: person-years; sojourn times in the various states, expected at birth; expected sojourn times at various ages. }
\author{Frans Willekens}
\seealso{The multistate life table is computed using the function MSLT.S from the Biograph package. }
\examples{
data(rates)
z <- Multistate (rates)
}