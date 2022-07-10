#' Retrieves ID of mother of ego
#' 
#' Retrieves the ID of mother of ego or mothers of vector of egos
#' 
#' 
#' @param idego ID
#' @param dLH Name of database. If missing, dataLH_F is used.
#' @param keep_ego Option to show ID of ego together with ID of mother
#' @return ID of mother or (if keep_ego=TRUE, object with ID of ego and ID of
#' mother). Returns NA if ID of mother is not included in the database
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH_F,package = "Families")
#' IDmother (sample (dataLH_F$ID,10))
#' IDmother(sample (dataLH_F$ID,10),keep_ego=TRUE)
#' 
#' @export IDmother
IDmother <- function (idego,dLH,keep_ego=FALSE)
#  dLH$IDmother[dLH$IDmother[24111]]
{  if (length(idego)==0) return (numeric(0))

#   NO!!!  assign("dLH", utils::data(dLH), envir = globalenv())
#    ls(globalenv())
#  before call function: utils::data(dLH)

if (missing(dLH)) 
   { dLH <- Families::dataLH_F
   } 
  # idego: ID of children
  # if child=1, output is data frame with ID mother and IDego (=child)

id2 <- idego  
if (is.data.frame(id2)) {if (is.na(idego[1,1])) return(NA)} else {if (all(is.na(idego))) return (NA)}
if (keep_ego) child <- 1 else child <- 0
# NOTE: idego produced by: idego <- IDch(IDego,mother=1)
if (is.data.frame(id2)) # First col is mother and second is child (also after IDmother)
   { idmother <- id2[,"IDmother"]
     idEgo <- id2[,"IDego"] 
    # children of IDmother
    idgm <- dLH$IDmother[idmother] # 1126 values of idego, of which 5620 females and 5003 of them with children (idch has 5003 values, about half are males). 
    idm <- cbind (IDgm=idgm,id2)
   } else 
   { idEgo=idego 
     # children of idEgo
     idm <- dLH$IDmother[idego]       # which(dLH$ID%in%idEgo)]
     if  (child==1)
     { idm <- data.frame(IDmother=idm,IDego=idego) 
     }
   } 
  return(idm)
}
