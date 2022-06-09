#' Retrieves ID of father of ego
#' 
#' Function to retrieve the ID of father of ego or fathers of vector of egos
#' 
#' 
#' @param id ID
#' @param dataLH Datbase. If missing, datc = dataLH
#' @param keep_ego Option to link show ID of ego together with ID of father
#' @return ID of father or (if keep_ego=TRUE, object with ID of ego and ID of
#' father). Returns NA if ID of father is not included in the database
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH)
#' IDfather (sample (dataLH$ID,10),dataLH)
#' 
#' @export IDfather
IDfather <-
function(id,dataLH,keep_ego=FALSE)
{ id2 <- id  
# utils::globalVariables("datag")
if (is.data.frame(id2)) # First col is mother and second is child (also after IDmother)
   { idfather <- id2[,"IDfather"]
     idEgo <- id2[,"IDego"] 
    # grandmother and grandfather
    idgm <- dataLH$IDmother[idfather] # 1126 values of id, of which 5620 females and 5003 of them with children (idch has 5003 values, about half are males). 
    idgf <- dataLH$IDfather[idfather]
    idf <- cbind (IDgm=idgm,IDgf=idgf,IDm=IDmother(id,dataLH),IDf=idfather)
   } else 
   { # children of idEgo
     idf2 <- dataLH$IDfather[id]
     idf <- IDpartner (IDmother(id,dataLH),dataLH)
     if  (keep_ego)
     { idf <- data.frame(IDfather=idf,IDego=id) 
     }
   } 
  return(idf)
}
