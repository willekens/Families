#' Retrieves ID of children of ego
#' 
#' Retrieves ID of children of ego or children of vector of egos
#' 
#' 
#' @param id ID of ego(s)
#' @param dataLH Database.
#' @param keep_ego Option to link show ID of ego together with ID of mother
#' @return ID of children or (if keep_ego=TRUE, data frame with ID of ego and
#' ID of children). If ego has no children or IDs of children are not included
#' in database, numeric(0) is returned and the message "No (grand)children of
#' ego in database". If keep_ego=TRUE, an data frame is returned with the
#' following columns: IDego, ID of mother of children, ID of father of
#' children, ID of children, character sequence denoting the sex of parent and
#' sex of child.
#' @author Frans Willekens
#' @examples
#' 
#' data(dataLH)
#' IDch(1,dataLH)
#' IDch(sample (dataLH$ID,10),dataLH,keep_ego=TRUE)
#' 
#' @export IDch
IDch <-
function (id,dataLH,keep_ego=FALSE)
{ # test: run first id=IDmother(10020,child=1) => is data frame
  # mother = 1: include info on mother
  # ego=1 : include ego (and siblings) in list of children
# id=id of ego
# If id is data frame created by IDch, first col is ID of grandmother and second col is ID of mother

 #  utils::globalVariables("dataLH")
if (any(is.na(id))) return (NA)

if (is.data.frame(id)==FALSE)
  { idchm <- dataLH$ID[dataLH$IDmother%in%id]
    dataLH$IDfather <- IDpartner(dataLH$IDmother,dataLH) 
    idchf <- dataLH$ID[dataLH$IDfather%in%id]
    idch <- idch2 <- c(idchm,idchf)
  }
if (length(idch)==0) 
      { idch2 <-  NA
        return (idch2)
      }
if (is.data.frame(id)) 
   {  idgch <- IDgch(unique(id$IDego),dataLH,keep_ego=TRUE)
      if (keep_ego==FALSE)
        { idgch <- idgch$grandchild  }
      return (idgch)
   }
if (is.data.frame(id)==FALSE & keep_ego==TRUE)
  { # sex of ego
       # ego is female
    idchm <- IDch(id[dataLH$sex[id]=="Female"],dataLH)
           # mother is IDmother(idchm)
      # ego is male
    k <- which (dataLH$sex[id]=="Male")
    idchf <- IDch(id[k],dataLH)
           # father is IDfather(idchf)
    
    if (all(is.na(idchf))) idchf <- numeric(0) 
    idego2 <- c(IDmother(idchm,dataLH),idchf)
    idego2 <- stats::na.omit(idego2)
    idchTab <- data.frame (IDego=idego2,IDmother=IDmother(idch,dataLH),IDfather=IDpartner(IDmother(idch,dataLH),dataLH),
          IDch=idch)
  # idchTab$sch <- NA
  # idchTab$sch[IDmother(idch)%in%id]  <- "F"
  # idchTab$sch[IDfather(idch)%in%id]  <- "M"
    # =========  Sequence  =============
    nam <- c("M","F")
    sego <- as.numeric(dataLH$sex[idchTab$IDego])
    z <- data.frame(a=nam[sego],c=nam[as.numeric(dataLH$sex[idch])])
    zz <- apply(z,1,function(x) paste(x,collapse="") )
    idchTab$sch <- NULL
   idchTab$path=zz
    idch2 <- noquote(idchTab)
  }
 # if (length(idch2)==0) return("Children of idego not in database")
 if (length(idch2)==0) print ("No (grand)children of ego in database")
 return (idch2)
}
