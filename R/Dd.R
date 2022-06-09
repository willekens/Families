#' Retrieves the date(s) of death in decimal format
#' 
#' Retrieves the date(s) of death from the database
#' 
#' 
#' @param id vector of IDs of egos
#' @param dataLH Name of database. If absent, the name 'dataLH' is used
#' @return Returns the date of death
#' @author Frans Willekens
#' @examples
#' 
#' # Date of death of first individual in database
#' data(dataLH)
#' Dd(1,dataLH) 
#' 
#' @export Dd
Dd <-
function (id,dataLH)
{ dd <- dataLH$ddated[id]
  return (dd)
}
