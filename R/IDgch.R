#' Retrieves IDs of grandchildren of ego
#' 
#' Retrieves IDs of grandchildren of vector of egos
#' 
#' IDgch uses IDch
#' 
#' @param id vector of IDs of egos
#' @param dataLH Database. If missing, datac=datab
#' @param keep_ego Option to show ID of ego together with ID of mother
#' @return ID of grandchildren or (if keep_ego=TRUE, data frame with ID of
#' members of multiple generations). If ego has no grandchildren or IDs of
#' grandchildren are not included in database, numeric(0) is returned and the
#' message "No (grand)children of ego in database". If keep_ego=TRUE, an data
#' frame is returned with the following columns: IDego, ID of ego's child that
#' is mother or father of grandchildren, IDs of mother and father of
#' grandchildren, ID of grandchildren, character sequence denoting the sex of
#' grandparent, parent and sex of child.
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH)
#' # Grandchildren of ego with ID 1
#' IDgch(1,dataLH)
#' # Grandchildren of member of first generation
#' IDgch(sample(dataLH$ID[dataLH$gen==1],3),dataLH,keep_ego=TRUE)
#' 
#' @export IDgch
IDgch <-
function(id,dataLH,keep_ego=FALSE)
{ # Grandchildren of ego
  # id=idch
  if (any(is.na(id))) return (NA)
  
if (is.data.frame(id)==FALSE & keep_ego==FALSE) 
      { idgch <- IDch(IDch(id,dataLH),dataLH)
        return (idgch)
      }
if (is.data.frame(id)==FALSE & keep_ego==TRUE) 
     {  id <- IDch(id,dataLH,keep_ego=TRUE)
     }

if (is.data.frame(id)==TRUE | keep_ego==TRUE)
 { # id <- IDch(id,keep_ego=TRUE)
   # idch <- IDch(id$IDego,keep_ego=TRUE)
   if (length(id)<=1) if (is.na(id)) return(NA) # no children
   idgch <- IDch(id$IDch,dataLH,keep_ego=TRUE)
  idgch$test <- id$IDch[match(idgch$IDego,id$IDch)]
  #  zz$test2 <- z$IDch[match(zz$IDego,z$IDch)]
  idgch <- idgch[,c(6,1:5)]
   nam <- c("M","F")
   idgch$path2 <- nam[as.numeric(dataLH$sex[idgch$test])]
   idgch$path <- paste0 (idgch$path2,idgch$path)
   idgch$path2 <- NULL
   colnames(idgch) <- c("IDego","child","IDmother","IDfather","grandchild","path")
   idgchTab <- idgch

   return (idgchTab)
  }
}
