#' Multistate life table
#' 
#' Computes fertility table by birth order
#' 
#' 
#' @param rates rates by age and sex and birth rates by age and birth order (or parity)
#' @param mortality Indicator variable. Mortality accounted for if mortality=1, else mortality omitted.
#' @return A list of two objects: 
#' item{S}{the multistate survival function (S) and multistate transition probabilities (P)}
#' item{mslt}{other measures of the multistate life table: person-years (L); expectation at birth of sojourn times in the various states (e0); expectation at age x of the remaining expected sojourn times in the various states: population-based measures (e.p); expectation at age x of the remaining expected sojourn times in the various states: status-based measures (e.p)} 
#' @details
#' The multistate life table is computed using the functions MSLT.S and MLST.e from the Biograph package. The two functions are included in the Multistate function as MSLT_S and MSLT_e.
#' @author Frans Willekens
#' @examples
#' 
#' data(rates,package = "Families")
#' z=Multistate(rates)
#' 
#' @export Multistate

Multistate <- function (rates,mortality=1)
{

MSLT_S <-
function (rates)  # replaced by M1 destin = 3th dimension
{ numstates <- dim(rates)[2]
  P<- apply(rates,1,function(x) 
	  MatrixExp(x,t=1,n=5,k=3,method="series"))
  zz <- array (P,c(numstates,numstates,nrow(rates)))
  P2 <- aperm(zz,c(3,1,2))  # P2 Age,dest,origin
 dimnames(P2) <- dimnames(rates)
  # S has the transition probabilities from each origin state to the different
  # destination states in conseciutive rows.
  # Ages are in columns
  # The following code rearranges the probabilities 
 S <- array (0,dim=c(nrow(rates),numstates,numstates))
 dimnames (S) = dimnames(rates)  # S Age,dest,orig
 S[1,,] <- diag(1,nrow=numstates,ncol=numstates)
 P3 <- P2   # aperm (P2,c(1,3,2))
 for (ix in 1:(nrow(rates)-1))
  { S[ix+1,,] <- P3[ix,,] %*% S[ix,,]
  }
 # apply(S,c(1,3),sum)  sum = 1 
  class(S) <- "MSLT_S"
  return(list (S=S,
               P=P3))
}

MSLT_e <- function (SS,radix)
{ 	S<-SS$S
	nage <- nrow(S)
   iradix <- which(radix>0)
   lt <- apply(S[,,iradix],1,sum) # total survival prob: starts in iradix (1)
    namstates <- unlist(unname(dimnames(S)[2]))
    numstates <- length (namstates)
   LL <- array(0,c(nage,numstates,numstates))
   dimnames(LL) <- dimnames(S)
   for (ix in 1:(nage-1))
   { LL[ix,,] <-  0.5 * (S[ix,,]+S[ix+1,,])
   }# Expected sojourn time beyond age 0
    e0 <- apply(LL,c(2,3),sum)
 #  population based: average person aged 50
 #   e50.p <- apply (LL[51:nage,,],c(2,3),sum)/lt[51]
 # status-based: by status at age 50
 #   e50.s <- apply (LL[51:nage,,],c(2,3),sum)%*%solve(S[51,,])
    e.p <- array(NA,dim=c(nage,numstates,numstates),dimnames=dimnames(S))
    e.s <- e.p
    for (ix in 1:(nage-1))
    {  e.p[ix,,] <- apply (LL[ix:nage,,],c(2,3),sum)/lt[ix]}
    
    LLL <- LL
    for (ix in 1:(nage-1))
    { S[ix,,] <- 0 
      diag(S[ix,,]) <- 1
      for (iy in ix:(nage-1))
      {  S[iy+1,,] <- SS$P[iy,,]%*%S[iy,,]
      	 LLL[iy,,] <- 0.5 * (S[iy,,]+S[iy+1,,])
      }
      e.s[ix,,] <- apply(LLL[ix:nage,,],c(2,3),sum)
    }
     #  zx <- det(S[ix,,])
     #  if (zx < 1e-16) {e.s[ix,,] <- NA} else
     #    {e.s[ix,,] <- apply (LL[ix:nage,,],c(2,3),sum)%*%solve(S[ix,,])}
    
  return (list(L = LL,
               e0=e0, 
               e.p = e.p,
               e.s=e.s ))
}


ages <- as.numeric(dimnames(rates$ratesM)$Age)
if (mortality==0)
  {rates$ASDR[1:length(ages),2] <- 0}
rat <- array(data=0,dim=c(nrow(rates$ASFR),6,6),
        dimnames=list(Age=rownames(rates$ASFR),Destination=0:5,Origin=0:5))
for (i in 1:5) { rat[,i+1,i] <- rates$ASFR[,i]}
#rat[,6,6] <- rates$ASFR[,5]
for (i in 1:5)
{ rat[,i,i] <- -rat[,i+1,i]-rates$ASDR[1:length(ages),2]
}
rat[,6,6] <- -rates$ASDR[1:length(ages),2]
    S <- MSLT_S(rat)
    radix <- c(10000,0)
    mslt <- MSLT_e (S,radix)
return(list(S=S,
            mslt=mslt))	
}