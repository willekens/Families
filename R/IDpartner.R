#' Retrieves ID of partner of ego or allocate partner to ego
#' 
#' Retrieves ID of partners of vector of egos or randomly allocates partners to
#' egos
#' 
#' 
#' @param idego vector of ID of egos. If idego is missing, then the function
#' allocates partners (from opposite sex) to egos. The allocation is random.
#' @param dataLH Database. If missing, database 'datap' is used.
#' @return IDs of partners. If the argument idego is missing, then a data frame
#' similar to 'dataLH' is returned with IDs of partners completed.
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH)
#' IDpartner(idego=1,dataLH)
#' # Allocate partner to egos with ID 4,9,30.
#' IDpartner(idego=dataLH$ID[c(4,9,30)],dataLH)  
#' 
#' @export IDpartner
IDpartner <-
function(idego=NULL,dataLH)
{  # Two uses
  # print (idego)
   #  if id=NULL: IDpartner of all egos determined and stored in IDpartner column of dataLH (datag)
   # if id=idego: IDpartner (column in datag must exist)

if (is.null(idego)) # Allocate partners
{ if (max(dataLH$gen) - min (dataLH$gen) > 0)  
       { print ("Partners already allocated")
         return (dataLH)}
nf <- length(dataLH$ID[dataLH$sex=="Female" & is.na(dataLH$IDpartner)])
nm <- length(dataLH$ID[dataLH$sex=="Male" & is.na(dataLH$IDpartner)])
# Number of females  may exceed number of males
nsample <- min(nf,nm)
if (nsample==0) 
       { print ("Partners already allocated")
         return (dataLH)}
print (paste("nsample =",c(nsample,nm,nf)))
if (nsample < nf)
{id <- sample(dataLH$ID[dataLH$sex=="Female" & is.na(dataLH$IDpartner)],nsample,replace=FALSE)
 dataLH$IDpartner[dataLH$sex=="Male" & is.na(dataLH$IDpartner)] <- id 
 xx <- subset (dataLH$ID,dataLH$sex=="Male" & !is.na(dataLH$IDpartner))
 partners <- cbind (female=id,male=xx)
} else
{id <- sample(dataLH$ID[dataLH$sex=="Male" & is.na(dataLH$IDpartner)],nsample,replace=FALSE)
 dataLH$IDpartner[dataLH$sex=="Female" & is.na(dataLH$IDpartner)] <- id 
 xx <- subset (dataLH$ID,dataLH$sex=="Female" & !is.na(dataLH$IDpartner))
 partners <- cbind (male=id,female=xx)}

for (i in 1:nrow(partners))
{ dataLH$IDpartner[dataLH$ID==partners[i,1]] <-partners[i,2]
}
which (dataLH$sex=="Female" & is.na(dataLH$IDpartner))
which (dataLH$sex=="Male" & is.na(dataLH$IDpartner))
} # end if is.null
if (!is.null(idego)) # Get ID of partner
{ idPartner <- dataLH$IDpartner[idego]
 # dataLH <-idPartner
}
if (!is.null(idego)) dataLH <- idPartner

return (dataLH)
}
