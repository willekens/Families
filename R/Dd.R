#' Retrieves the date(s) of death in decimal format
#' 
#' Retrieves the date(s) of death from the database
#' 
#' 
#' @param idego vector of IDs of egos
#' @param dLH Name of database. If dLH is missing, dataLH_F is used.
#' @return Returns the date of death
#' @author Frans Willekens
#' @examples
#' 
#' # Date of death of first individual in database
#' data(dataLH_F,package = "Families")
#' Dd(idego=1) 
#' 
#' @export Dd
Dd <- function (idego,dLH)
{if (missing(dLH)) 
   {  dLH <- Families::dataLH_F
   } 
   dd <- dLH$ddated[idego]
  return (dd)
}
