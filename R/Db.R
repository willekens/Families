#' Retrieves the date(s) of birth in decimal format
#' 
#' Retrieves the date(s) of birth from the database
#' 
#' 
#' @param id vector of IDs of egos
#' @param dataLH Name of database. If absent, the name 'dataLH' is used.
#' @return Returns the dates of birth
#' @author Frans Willekens
#' @examples
#' 
#' # Date of birth of first individual in database
#' data(dataLH)
#' Db(1,dataLH) 
#' 
#' @export Db
Db <-
function (id,dataLH)
{ dbd <- dataLH$bdated[id]
  return (dbd)
}
