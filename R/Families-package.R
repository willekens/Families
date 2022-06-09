

#' dataLH data
#' 
#' simulated population of four generations
#' 
#' 
#' @name dataLH
#' @docType data
#' @format A data frame with data on 1000 individuals.  \describe{
#' \item{list("ID")}{Identification number} \item{list("gen")}{Generation}
#' \item{list("sex")}{Sex. A factor with levels \code{Males} \code{Females}}
#' \item{list("bdated")}{Date of birth (decimal date)}
#' \item{list("ddated")}{Date of death (decimal date)} \item{list("x_D")}{Age
#' at death (decimal number)} \item{list("IDpartner")}{ID of partner}
#' \item{list("IDmother")}{ID of mother} \item{list("IDfather")}{ID of father}
#' \item{list("jch")}{Child's line number in the household}
#' \item{list("nch")}{Number of children ever born} \item{list("id.1")}{ID of
#' first child} \item{list("id.2")}{ID of 2nd child} \item{list("id.3")}{ID of
#' 3rd child} \item{list("id.4")}{ID of 4th child} \item{list("id.5")}{ID of
#' 5th child} \item{list("id.6")}{ID of 6th child} \item{list("id.7")}{ID of
#' 7th child} \item{list("id.8")}{ID of 8th child} \item{list("id.9")}{ID of
#' 9th child} \item{list("age.1")}{Age of mother at birth of first child}
#' \item{list("age.2")}{Age of mother at birth of 2nd child}
#' \item{list("age.3")}{Age of mother at birth of 3rd child}
#' \item{list("age.4")}{Age of mother at birth of 4th child}
#' \item{list("age.5")}{Age of mother at birth of 5th child}
#' \item{list("age.6")}{Age of mother at birth of 6th child}
#' \item{list("age.7")}{Age of mother at birth of 7th child}
#' \item{list("age.8")}{Age of mother at birth of 8th child}
#' \item{list("age.9")}{Age of mother at birth of 9th child} }
#' @source Simulation uses period mortality rates and fertility rates by birth
#' order from the United States 2019. The data are downloaded from the Human
#' Mortality Database (HMD) and the Human Fertility Database (HFD).
#' @keywords datasets
NULL





#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "Families")\Sexpr{tools:::Rd_package_title("Families")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "Families")\Sexpr{tools:::Rd_package_description("Families")}
#' 
#' The DESCRIPTION file:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
#' "Families")\Sexpr{tools:::Rd_package_DESCRIPTION("Families")}
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
#' "Families")\Sexpr{tools:::Rd_package_indices("Families")}
#' 
#' @name Families-package
#' @aliases Families-package FamiliesPop
#' @docType package
#' @author Frans Willekens <Willekens@@nidi.nl>
#' @keywords Family demography
NULL





#' rates data
#' 
#' Mortality rates by age and sex: fertility rates by age and birth order
#' 
#' 
#' @name rates
#' @docType data
#' @format A list of three objects with mortality and fertility rates.
#' \describe{ \item{list("ASDR")}{Mortality rates}
#' \item{list("ASFR")}{Fertility rates} \item{list("ratesM")}{Multistate
#' transition rates} }
#' @source The data are downloaded from the Human Mortality Database (HMD) and
#' the Human Fertility Database (HFD). Country: USA. Year: 2019
#' @keywords datasets
NULL



