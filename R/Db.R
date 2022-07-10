#' Retrieves the date(s) of birth in decimal format
#' 
#' Retrieves the date(s) of birth from the database
#' 
#' 
#' @param idego vector of IDs of egos
#' @param dLH Name of database. If dLH is missing, dataLH_F is used.
#' @return Returns the dates of birth
#' @author Frans Willekens
#' @examples
#' 
#' # Date of birth of first individual in database
#' data(dataLH_F,package = "Families")
#' Db(idego=1) 
#' 
#' @export Db
Db <- function (idego,dLH)
{if (missing(dLH)) 
   {  dLH <- Families::dataLH_F
   } 
  dbd <- dLH$bdated[idego]
  return (dbd)
}
