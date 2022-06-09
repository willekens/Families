#' Retrieves ID of mother of ego
#' 
#' Retrieves the ID of mother of ego or mothers of vector of egos
#' 
#' 
#' @param id ID
#' @param dataLH Datbase. If missing, datc = dataLH
#' @param keep_ego Option to show ID of ego together with ID of mother
#' @return ID of mother or (if keep_ego=TRUE, object with ID of ego and ID of
#' mother). Returns NA if ID of mother is not included in the database
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH)
#' IDmother (sample (dataLH$ID,10),dataLH)
#' IDmother(sample (dataLH$ID,10),dataLH,keep_ego=TRUE)
#' 
#' @export IDmother
IDmother <- function (id,dataLH,keep_ego=FALSE)
#  dataLH$IDmother[dataLH$IDmother[24111]]
{ # id: ID of children
  # if child=1, output is data frame with ID mother and IDego (=child)

id2 <- id  
# utils::globalVariables("dataLH")
if (is.data.frame(id2)) {if (is.na(id[1,1])) return(NA)} else {if (all(is.na(id))) return (NA)}
if (keep_ego) child <- 1 else child <- 0
# NOTE: id produced by: id <- IDch(IDego,mother=1)
if (is.data.frame(id2)) # First col is mother and second is child (also after IDmother)
   { idmother <- id2[,"IDmother"]
     idEgo <- id2[,"IDego"] 
    # children of IDmother
    idgm <- dataLH$IDmother[idmother] # 1126 values of id, of which 5620 females and 5003 of them with children (idch has 5003 values, about half are males). 
    idm <- cbind (IDgm=idgm,id2)
   } else 
   { idEgo=id 
     # children of idEgo
     idm <- dataLH$IDmother[idEgo]
     if  (child==1)
     { idm <- data.frame(IDmother=idm,IDego=id) 
     }
   } 
  return(idm)
}
