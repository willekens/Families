#' Retrieves ID of father of ego
#' 
#' Function to retrieve the ID of father of ego or fathers of vector of egos
#' 
#' 
#' @param idego ID
#' @param dLH Name of database. If missing, dataLH_F is used.
#' @param keep_ego Option to link show ID of ego together with ID of father
#' @return ID of father or (if keep_ego=TRUE, object with ID of ego and ID of
#' father). Returns NA if ID of father is not included in the database
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH_F,package = "Families")
#' IDfather (idego=sample (dataLH_F$ID,10))
#' 
#' @export IDfather
IDfather <-
function(idego,dLH,keep_ego=FALSE)
{if (missing(dLH)) 
   {  dLH <- Families::dataLH_F
   } 
   id2 <- idego  
# utils::globalVariables("datag")
if (is.data.frame(id2)) # First col is mother and second is child (also after IDmother)
   { idfather <- id2[,"IDfather"]
     idEgo <- id2[,"IDego"] 
    # grandmother and grandfather
    idgm <- dLH$IDmother[idfather] # 1126 values of idego, of which 5620 females and 5003 of them with children (idch has 5003 values, about half are males). 
    idgf <- dLH$IDfather[idfather]
    idf <- cbind (IDgm=idgm,IDgf=idgf,IDm=IDmother(idego),IDf=idfather)
   } else 
   { # children of idEgo
     idf2 <- dLH$IDfather[idego]
     idf <- IDpartner (IDmother(idego))
     if  (keep_ego)
     { idf <- data.frame(IDfather=idf,IDego=idego) 
     }
   } 
  return(idf)
}
