#' Retrieves ID of partner of ego or allocate partner to ego
#' 
#' Retrieves ID of partners of vector of egos or randomly allocates partners to
#' egos
#' 
#' 
#' @param idego vector of ID of egos. If idego is missing, then the function
#' allocates partners (from opposite sex) to egos. The allocation is random.
#' @param dLH Name of database. If missing, dataLH_F is used.
#' @return IDs of partners. If the argument idego is missing, then a data frame
#' similar to 'dLH' is returned with IDs of partners completed.
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH_F,package = "Families")
#' IDpartner(idego=1)
#' # Allocate partner to egos with ID 4,9,30.
#' IDpartner(idego=dataLH_F$ID[c(4,9,30)])  
#' 
#' @export IDpartner
IDpartner <-
function(idego,dLH)
{if (missing(dLH)) 
   {  dLH <- Families::dataLH_F
   } 
    # Two uses
   #  if id=NULL: IDpartner of all egos determined and stored in IDpartner column of dLH (datag)
   # if id=idego: IDpartner (column in datag must exist)

if (missing(idego)) # Allocate partners
{ if (max(dLH$gen) - min (dLH$gen) > 0)  
       { message ("Partners already allocated")
         return (dLH)}
nf <- length(dLH$ID[dLH$sex=="Female" & is.na(dLH$IDpartner)])
nm <- length(dLH$ID[dLH$sex=="Male" & is.na(dLH$IDpartner)])
# Number of females  may exceed number of males
nsample <- min(nf,nm)
if (nsample==0) 
       { message ("Partners already allocated")
         return (dLH)}
#  message (paste("nsample =",c(nsample,nm,nf)))
if (nsample < nf)
{id <- sample(dLH$ID[dLH$sex=="Female" & is.na(dLH$IDpartner)],nsample,replace=FALSE)
 dLH$IDpartner[dLH$sex=="Male" & is.na(dLH$IDpartner)] <- id 
 xx <- subset (dLH$ID,dLH$sex=="Male" & !is.na(dLH$IDpartner))
 partners <- cbind (female=id,male=xx)
} else
{id <- sample(dLH$ID[dLH$sex=="Male" & is.na(dLH$IDpartner)],nsample,replace=FALSE)
 dLH$IDpartner[dLH$sex=="Female" & is.na(dLH$IDpartner)] <- id 
 xx <- subset (dLH$ID,dLH$sex=="Female" & !is.na(dLH$IDpartner))
 partners <- cbind (male=id,female=xx)}

for (i in 1:nrow(partners))
{ dLH$IDpartner[dLH$ID==partners[i,1]] <-partners[i,2]
}
which (dLH$sex=="Female" & is.na(dLH$IDpartner))
which (dLH$sex=="Male" & is.na(dLH$IDpartner))
} # end if is.null
if (!is.null(idego)) # Get ID of partner
{ idPartner <- dLH$IDpartner[idego]
 # dLH <-idPartner
}
if (!is.null(idego)) dLH <- idPartner

return (dLH)
}
