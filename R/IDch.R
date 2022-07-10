#' Retrieves ID of children of ego
#' 
#' Retrieves ID of children of ego or children of vector of egos
#' 
#' 
#' @param idego ID of ego(s)
#' @param dLH Name of database. If dLH is missing, dataLH_F is used.
#' @param keep_ego Option to link show ID of ego together with ID of mother
#' @return ID of children. If ego has no children or IDs of children are not included
#' in database, numeric(0) is returned. If keep_ego=TRUE, a data frame is returned with the
#' following columns: IDego, ID of mother of children, ID of father of
#' children, ID of children, sex of children. 
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH_F,package = "Families")
#' IDch(idego=1)
#' id <- sample (dataLH_F$ID[dataLH_F$gen==1],10)
#' IDch(idego=sort(id),keep_ego=TRUE)
#' 
#' @export IDch
IDch <-
function (idego,dLH,keep_ego=FALSE)
{
# idego is a vector of identification numbers (IDs); get the vector of IDs of children
# ================   determine database   ================
# utils::globalVariables("dLH") 
if (missing(dLH)) 
   {  dLH <- Families::dataLH_F
   } 
# ===============  Check idego   ================
# If idego is missing or idego is NA, skip
if (any(is.na(idego))) return (NA)
if (length(idego)==0) 
      { idch2 <-  NA
        return (idch2)
      }

dframe <- FALSE
if (is.data.frame(idego))   
       { dframe <- TRUE
         idego_dframe <- idego
         idego <- idego_dframe$Child
       }

idch <- dLH$ID[dLH$IDmother%in%idego | dLH$IDpartner[dLH$IDmother]%in%idego]

if (keep_ego==FALSE)
     { idch2 <- idch
     }  else
{jj <- dLH$IDmother[idch]%in%idego
 z <- data.frame(ego_is_mother=jj,Mother=dLH$IDmother[idch],Father=dLH$IDpartner[dLH$IDmother[idch]], Child=idch)
 k <- dLH$IDmother[idch][dLH$IDmother[idch]%in%idego]
 # sex of individuals
 nam <- c("M","F")
 sexch <- nam[as.numeric(dLH$sex[idch])]
 z$sexego <- NA
 z$sexego[jj] <- nam[2]
 z$sexego[jj==FALSE] <- nam[1]
 # sex of children
 z$sex_child <- sexch   
 idch2 <- z 
} 
if (dframe & keep_ego)
{ zz <- z
  zz$Maternalgm <- dLH$IDmother[zz$Mother]
  zz$Maternalgf <- dLH$IDpartner[dLH$IDmother[zz$Mother]]
  zz$Paternalgm <- dLH$IDmother[zz$Father]
  zz$Paternalgf <- dLH$IDpartner[dLH$IDmother[zz$Father]]
  idch2 <- zz
}
 return (idch2)
}
