# Mean ages at death and probabilities of surviving to selected ages, by sex
#' 
#' Computes (a) Life expectancy at birth, (b) Probability of surviving at age
#' 65, and (c) Probability of surviving at age 85
#' 
#' 
#' @param dLH The name of the database. If missing, dataLH_F is used.
#' @return \item{e0}{Mean ages at death} \item{Prob65}{Probability of surviving
#' at age 65} \item{Prob85}{Probability of surviving at age 85}
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH_F,package = "Families")
#' e0(dLH=dataLH_F)
#' 
#' @export e0
e0 <- function (dLH)
{ 
d <- base::subset(dLH,dLH$gen==1)
 e0 <-  aggregate(x=d$x_D,by=list(age=d$sex),mean)

# Prob of surviving at age 65, by sex
dd <- base::subset (d,d$x_D>=65)
aggregate(x=d$ID,by=list(age=d$sex),function(x) length(x))
table (d$sex)
table (dd$sex)
p65 <- table (dd$sex) / table (d$sex)

dd <- base::subset (d,d$x_D>=85)
p85 <- table (dd$sex) / table (d$sex)

# Other approach
mean(dLH$x_D[dLH$gen==1 & dLH$sex=="Female"])
mean(dLH$x_D[dLH$gen==1 & dLH$sex=="Male"])

return (list (e0=e0,
              Prob65=p65,
              Prob85=p85))
}
