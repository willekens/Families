## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Attach the package to the search path
library (Families)
# Load the data in your workspace (environment:R_GlobalEnv)
data(dataLH_F,package = 'Families')
data(rates,package = 'Families')

dLH <-  dataLH_F
# Number of generations in the dataset
ngen <- max(unique (dLH$gen))

## -----------------------------------------------------------------------------
addmargins(table (Generation=dLH$gen,Sex=dLH$sex))

## -----------------------------------------------------------------------------
# Create local copies of the functions (in workspace)
IDmother <- Families::IDmother
IDfather <- Families::IDfather
IDpartner <- Families::IDpartner
IDch <- Families::IDch
Db <- Families::Db
Dd <- Families::Dd

## -----------------------------------------------------------------------------
base::set.seed(30)
idego <- sample (dLH$ID[dLH$gen==3],3)
z <- dLH[c(idego,IDmother(idego),IDmother(IDmother(idego))),]
rownames(z) <- NULL
z[,1:9]

## -----------------------------------------------------------------------------
z[,c(10:14,21:23)]

## -----------------------------------------------------------------------------
IDmother(IDmother(idego,keep_ego=TRUE))

## -----------------------------------------------------------------------------
# Children
IDch(id=2)
# Grandchildren
IDch(IDch(id=2))

## -----------------------------------------------------------------------------
# Select all females in generation 1 
idego <- dLH$ID[dLH$gen==1 & dLH$sex=="Female"]
# IDs of children
idch <- IDch(idego)
# IDs of mother with children
idm <- unique(IDmother(idch))

## -----------------------------------------------------------------------------
addmargins (table(dLH$nch[dLH$gen==1 & dLH$sex=="Female"]))

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==1 & dLH$sex=="Female"]
# ID of children
idch <- IDch(idego,dLH)
# Date of birth of children
dbch <- dLH$bdated[idch]
# Create data frame
zz <-data.frame (ID=idch,dbch=dbch)
# Select, for each ego, child with lowest date of birth
ch_oldest=aggregate(zz,list(dLH$IDmother[idch]),function(x) min(x))
colnames(ch_oldest) <- c("idego","ID of oldest child","date of birth of oldest child")

## ---- echo=FALSE--------------------------------------------------------------
head(ch_oldest)

## -----------------------------------------------------------------------------
# Select, for each ego, child with highest date of birth
#    zz has ID of all children and dates of birth of children. 
#    They are used to select youngest child ever born (by mother)
ch_youngest=aggregate(zz,list(dLH$IDmother[idch]),function(x) max(x))
# Date of birth of mother
ch_youngest$db_mother <- Db(ch_youngest[,1])
# Age of mother at birth youngest child
ch_youngest$agem_chLast <- ch_youngest[,3] - ch_youngest$db_mother
colnames(ch_youngest) <- c("idego","ID_youngest_child","db_youngest_child","db_idego","agemLast")

## -----------------------------------------------------------------------------
  xmin <- 10
  xmax <- 50
  library(ggplot2)
  p <- ggplot(ch_youngest, aes(agemLast)) +  
               geom_histogram(aes(y=..density..), alpha=0.5, position="identity",bins=50)+  
               geom_density(alpha=0.2) +
            scale_x_continuous(breaks=seq(xmin,xmax,by=5)) +
            scale_y_continuous (breaks=seq(0,0.07,by=0.01)) +
            labs(x="Age")
  title <- paste ("Age of mother at birth of youngest child; ",attr(dLH,"country"),attr(dLH,"year") )
  p <- p + ggtitle(title) +
  theme(plot.title = element_text(size = 10, face = "bold"))
  p

## -----------------------------------------------------------------------------
Status_refageEgo <- function(refage_ego)
{ # IDs of egos (children)
  idego <- IDch(dLH$ID[dLH$gen==1 & dLH$sex=="Female"])
  # Is ego alive at reference age?
  alive_ego <- Dd(idego) >= Db(idego) + refage_ego
  # Number of children (ego) alive (=TRUE) and dead (=FALSE) at reference age
  t1 <- table (alive_ego)
  # IDs of egos alive at reference age
  idegoRefage <- idego[alive_ego]
  # Is mother alive at ego's reference age?
  alive_m <- Dd(IDmother(idego)) >=  Db(idego) + refage_ego
  # Number of mothers alive (=TRUE) or dead (=FALSE) at reference ages of egos
  t2 <- table (alive_m)
  aa <- list (idego=idego,
              alive_ego=alive_ego,
              alive_m=alive_m,
              LivingStatus_ch_refage=t1,
              LivingStatus_m_refage=t2)
  return(aa)
}
refage_ego <- 10
out <- Status_refageEgo (refage_ego)

## -----------------------------------------------------------------------------
round (length(out$alive_m[out$alive_m]) / length(out$alive_ego),2)

## -----------------------------------------------------------------------------
# Age of living mothers at refage of ego
idegoRefage <-  out$idego[out$alive_ego]
age_m <- (Db(idegoRefage) + refage_ego - Db(IDmother(idegoRefage)))[out$alive_m]
mean(age_m)
sd(age_m)
hist(age_m,main=paste ("Age of living mother at age ",refage_ego," of ego",sep=""),breaks=40,xlab="Age of living mother")
box()

## -----------------------------------------------------------------------------
refage_ego <- 65
out <- Status_refageEgo (refage_ego)
# Age of living mothers at refage of ego
idegoRefage <-  out$idego[out$alive_ego]
age_m <- (Db(idegoRefage) + refage_ego - Db(IDmother(idegoRefage)))[out$alive_m]
mean(age_m)
sd(age_m)
hist(age_m,main=paste ("Age of living mother at age ",refage_ego," of ego",sep=""),breaks=40,xlab="Age of living mother")
box()

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==3 & dLH$sex=="Female"]
age_m85 <- dLH$bdated[IDmother(idego)] + 85 - dLH$bdated[idego]
age_md <- dLH$ddated[IDmother(idego)] - dLH$bdated[idego]
d <- data.frame (idego,m85=age_m85,md=age_md)

## -----------------------------------------------------------------------------
library (ggplot2)
Plot_ages <- function (d)
{ # Age of ego at age 85 of mother and at death of mother
  dd <- reshape::melt.data.frame(d,id.vars="idego",measure.vars=c("m85","md"))
  colnames(dd)[2] <- "Age"
  xmin <- 10
  xmax <- 90
  p <- ggplot(dd, aes(x=value,color=Age,fill=Age)) +  
               geom_histogram(aes(y=..density..), alpha=0.5, position="identity",bins=50)+  
               geom_density(alpha=0.2) +
            scale_x_continuous(breaks=seq(xmin,xmax,by=10)) +
            scale_y_continuous (breaks=seq(0,0.07,by=0.01)) +
            xlab("Age")
  # Add median
  p <- p + theme(legend.position=c(0.76,0.99),legend.justification=c(0,1)) 
  title <- paste ("Age of ego at the 85th birthday of ego's mother and at death of mother; ",attr(dLH,"country"),attr(dLH,"year") )
  p <- p + ggtitle(title) +
  theme(plot.title = element_text(size = 10, face = "bold"))
 p
}
Plot_ages(d)

## -----------------------------------------------------------------------------
# Age at death
z <-  dLH$ddated[IDmother(idego,dLH)] - dLH$bdated[IDmother(idego,dLH)]
alive <- z >=85
duration <-  dLH$ddated[IDmother(idego,dLH)][alive] - (dLH$bdated[IDmother(idego,dLH)][alive] + 85)

## -----------------------------------------------------------------------------
# select one individual of generation 3 who is third child of the family as ego
idego <- sample(dLH$ID[dLH$gen==3 & dLH$jch==3],1)
# IDs of the children of ego's mother
IDch(IDmother(idego))
# ID of ego's siblings: children of ego's mother, excluding ego
IDch(IDmother(idego))[IDch(IDmother(idego))%in%idego==FALSE]

## -----------------------------------------------------------------------------
idego <- c(2723,2192,2291)
IDch(IDmother(idego))

## -----------------------------------------------------------------------------
IDch(IDmother(idego))[IDch(IDmother(idego))%in%idego==FALSE]

## -----------------------------------------------------------------------------
idego <- c(2723,2724,2192,2291)
IDch(IDmother(idego))[IDch(IDmother(idego))%in%idego==FALSE]

## -----------------------------------------------------------------------------
f <- function(idego)
      y <- IDch(IDmother(idego))[IDch(IDmother(idego))%in%idego==FALSE]
kk <- list()
for (i in 1:length(idego))
{ z <- f(idego[i])
  kk[[i]] <- z
}
names(kk) <- paste ("Siblings of reference person ",idego[1:length(idego)],sep="")
kk

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==2]
f <- function(idego)
      y <- IDch(IDmother(idego))[IDch(IDmother(idego))%in%idego==FALSE]
sib <- list()
for (ego in 1:length(idego))
{ z <- f(idego[ego])
  sib[[ego]] <- z
}

## -----------------------------------------------------------------------------
sib[[which(idego==1001)]]

## -----------------------------------------------------------------------------
z <- sapply(sib,function(x) length(x),simplify=TRUE)
percentage <- round (100 * table(z)/sum(table(z)),2)
percentage
mean <- mean(z,na.rm=TRUE)
sd <- sd(z,na.rm=TRUE)

## -----------------------------------------------------------------------------
# Ages of siblings at age 10 of ego
m <- as.list(idego)
# Merge two lists: list of egos and list of siblings of egos
zz <- base::Map(c,m,sib)
AgeSib <- function (zz,ageRef)
   { kk <- sapply (zz,function(x) 
      { # ageReference <- 30
        # In absence of siblings, skip 
        if (length(x)==1) mage <- NA else
        {
        # Date of birth of ego
        db_ego <- Db(x[1])
        # Dates of birth of siblings
        db_sib <- Db(x[2:length(x)])
        # Ages of siblings at reference age of ego
        age_sib <- db_ego+ageRef - db_sib
        # Omit negative values (siblings born after refAgeth birthday of ego)
        age_sib[age_sib<0] <- NA
        # Mean age of siblings at age ageReference of ego
        mage <- mean(age_sib,na.rm=TRUE) 
        }
      })
    return(kk)
   }
# Age distribution of siblings at age 10 of ego
mage_sib10 <- AgeSib(zz,ageRef=10)
# Age distribution of siblings at birth of ego
mage_sib0 <- AgeSib(zz,ageRef=0)

## -----------------------------------------------------------------------------
# par(mfrow=c(2,1))
# layout(matrix(1:2,nrow=1),widths=lcm(c(7,8)), heights=lcm(c(9,9)))
layout(matrix(1:2,nrow=1))
hist(mage_sib0,las=1,main="At birth",xlab="Age of siblings",cex.main=0.9,cex.lab=0.8)
hist(mage_sib10,las=1,main="At age 10",xlab="Age of siblings",cex.main=0.9,cex.lab=0.8)

## -----------------------------------------------------------------------------
# Select women in generation 2
id <- dLH$ID[dLH$gen==2 & dLH$sex=="Female"]
# Proporiton of women by number of children ever born
z <- table (dLH$nch[idego]) / sum(table (dLH$nch[idego]))
# Proportion of mothers by number of children ever born
zz <- round (100 * table (dLH$nch[idego])[2:6]/sum(table (dLH$nch[idego])[2:6]),2)

## -----------------------------------------------------------------------------
idego <- 2289
idgm <- IDmother(IDmother(idego))
idgch <- IDch(IDch(idgm))
idcousins <- idgch[idgch%in%IDch(IDmother(idego))==FALSE]
idcousins

## -----------------------------------------------------------------------------
idego <- sample(dLH$ID[dLH$gen==3],3)
idgm <- IDmother(IDmother(idego))
idcousins <- IDch(IDch(idgm))[IDch(idgm)%in%IDmother(idego)==FALSE]

## -----------------------------------------------------------------------------
idgm <- IDmother(IDmother(idego))
f <- function(id,idgm)  
            { grandch <- IDch(IDch(idgm))
              cous <- grandch[IDmother(grandch)%in%IDmother(id)==FALSE]
            }
idcousins <- list()
for (i in 1:length(idego))
{ z <- f(idego[i],idgm[i])
  idcousins[[i]] <- z
}

## -----------------------------------------------------------------------------
# IDs of members of the third generation
idego <- dLH$ID[dLH$gen==3]
# IDs of their grandmothers
idgm <- IDmother(IDmother(idego))
# IDs of their cousins
idcousins <- list()
for (i in 1:length(idego))
{ z <- f(idego[i],idgm[i])
  idcousins[[i]] <- z
}

## -----------------------------------------------------------------------------
z <- sapply(idcousins,function(x) length(x),simplify=TRUE)
percentage <- round (100 * table(z)/sum(table(z)),2)
percentage

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==3]
idgm <- IDmother(IDfather(idego))
idcousins <- list()
for (i in 1:length(idego))
{ z <- f(idego[i],idgm[i])
  idcousins[[i]] <- z
}
z <- sapply(idcousins,function(x) length(x),simplify=TRUE)
percentage <- round (100 * table(z)/sum(table(z)),2)
percentage

## -----------------------------------------------------------------------------
# ID of grandmother
idgm <- IDmother(IDmother(idego,dLH),dLH)
# Function of compute the IDs of grandmother's daughters, their partners, and the IDs of aunts
f <- function(id,idgm,dLH)  
            { iddaught <- c(IDch(idgm)[dLH$sex[IDch(idgm)]=="Female"],
              IDpartner(IDch(idgm)[dLH$sex[IDch(idgm)]=="Male"]))
              aunts <- iddaught[iddaught%in%IDmother(id)==FALSE]
            }

aunts <- list()
for (i in 1:length(idego))
{ z <- f(idego[i],idgm[i],dLH)
  # IDs of aunts
  aunts[[i]] <- z
}

## -----------------------------------------------------------------------------
z <- sapply(aunts,function(x) length(x),simplify=TRUE)
percentage <- round (100 * table(z)/sum(table(z)),2)
percentage

## -----------------------------------------------------------------------------
idego <- subset (dLH$ID,dLH$gen==1 & dLH$sex=="Female")
ages <- Db(IDch(idego))-Db(IDmother(IDch(idego)))

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==1 & dLH$sex=="Female"]
# IDs of grandchildren
idgch <- IDch(IDch(idego))
# Ages if egos at birth of grandchildren
ages <- Db(idgch) - Db(IDmother(IDmother(idgch)))

## -----------------------------------------------------------------------------
# Determine,, for each ego, the ID of grandchildren
z=IDch(IDch(idego,keep_ego=TRUE),keep_ego=TRUE)
# IDs of grandchildren
idgch <- z$Child
# IDs of the grandmothers
idgm_gch <- z$Maternalgm
# Add dates of birth of grandchildren
z$db_gch <- Db(idgch)
# Is grandmother alive at birth of the grandchild?
z$alive <- Dd(idgm_gch) > Db(idgch)
# Table of number of grandmother alive (=TRUE) or dead (=FALSE) at birth of grandchildren
table (z$alive)
# Age at grandmotherhood 
zz <- aggregate(z,by=list(z$Maternalgm),function(x) min(x))
# For each grandmother, the ID of first grandchild is
idgch1 <- zz$Child
# The age of grandmother at birth of first grandchild
agegm_gch1 <- Db(idgch1) - Db(IDmother(IDmother(idgch1)))
# Grandmother is alive at birth of first grandchild if zz$alive is 1
table (zz$alive)

## -----------------------------------------------------------------------------
# ID of females in first generation with children
idego <- dLH$ID[dLH$gen==1 & dLH$sex=="Female" & dLH$nch>=1]
# ID of first child
idch <- IDch(idego)  
idch1 <- idch[dLH$jch[idch]==1]
# ID of first grandchild
idgch <- IDch(IDch(idego))
zz <- data.frame(id=IDmother(IDmother(idgch)),idgch=idgch)
idgch1 <- aggregate(zz,by=list(zz$id),function(x) min(x))
# Create data frame with ID of mother, ID of first child and ID of first grandchild
d <- data.frame(idego=idego,idch1=idch1)
d$idgch1 <- NA
d$idgch1[idego%in%idgch1$id] <- idgch1$idgch
# Add age of ego at birth of first child
d$agem_ch1 <- Db(d$idch1) - Db(IDmother(d$idch1))
# Add age of ego at birth of first grandchild
d$agegm_gch1 <- NA
z1<- Db(d$idgch1)[!is.na(d$idgch1)]  
z2 <- Db(IDmother(IDmother(d$idgch1)))
z3 <- z2[!is.na(d$idgch1)]
d$agegm_gch1[!is.na(d$idgch1)]  <- z1-z3

## ----fig1, fig.height = 3, fig.width = 5,fig.align = "center"-----------------
colnames(d)[4:5] <- c("Motherhood","Grandmotherhood")
Plot_agesgm <- function (agem_ch1,agegm_gch1)
{ # Age of mother and grandmother at birth of child
    dd <- reshape::melt.data.frame(d,id.vars="idego",measure.vars=c("Motherhood","Grandmotherhood"))
  colnames(dd)[2] <- "Age"
  ddd <- dd[!is.na(dd$value),]
  xmin <- 10
  xmax <- 90
  p <- ggplot(ddd, aes(x=value,color=Age,fill=Age)) +  
               geom_histogram(aes(y=..density..), alpha=0.5, position="identity",bins=50)+  
               geom_density(alpha=0.2) +
            scale_x_continuous(breaks=seq(xmin,xmax,by=10)) +
            scale_y_continuous (breaks=seq(0,0.07,by=0.01)) 
  # Add median
  p <- p + theme(legend.position=c(0.76,0.99),legend.justification=c(0,1)) +
    theme(legend.title = element_text(colour="black", size=10,face="bold")) +
    theme(legend.text = element_text(colour="blue", size=10,face="plain"))
  title <- paste ("Age at motherhood and grandmotherhood; ",attr(dLH,"country"),attr(dLH,"year") )
  p <- p + ggtitle(title) +
  theme(plot.title = element_text(size = 10, face = "bold"))
 p
}

Plot_agesgm(agem_ch1,agegm_gch1)

## -----------------------------------------------------------------------------
yrs_gm <- ((Dd(d$idego)-Db(d$idego))-d$agegm_gch1)[!is.na(d$agegm_gch1)]

## ----fig2, fig.height = 3, fig.width = 5,fig.align = "center",warning=FALSE----
Plot_agesgm2 <- function (dLH)
{ # Age of mother and grandmother at birth of child
  id_ego <- dLH$ID
  agem <- dLH$bdated[id_ego]-dLH$bdated[IDmother(id_ego)]
  agegm <- dLH$bdated[id_ego]-dLH$bdated[IDmother(IDmother(id_ego))]
  d <- data.frame(id_ego=id_ego,mother=agem,grandmother=agegm)
  dd <- reshape::melt.data.frame(d,id.vars="id_ego",measure.vars=c("mother","grandmother"))
  colnames(dd)[2] <- "Age"
  xmin <- 10
  xmax <- 90
  p <- ggplot(dd, aes(x=value,color=Age,fill=Age)) +  
               geom_histogram(aes(y=..density..), alpha=0.5, position="identity",bins=50)+  
               geom_density(alpha=0.2) +
            scale_x_continuous(breaks=seq(xmin,xmax,by=10)) +
            scale_y_continuous (breaks=seq(0,0.07,by=0.01)) 
  # Add median
  p <- p + theme(legend.position=c(0.76,0.99),legend.justification=c(0,1)) 
  title <- paste ("Age of mother and grandmother at birth of child; ",attr(dLH,"country"),attr(dLH,"year") )
  p <- p + ggtitle(title) +
  theme(plot.title = element_text(size = 10, face = "bold"))
 p
}
Plot_agesgm2 (dLH)

## -----------------------------------------------------------------------------
idego <- dLH$ID[dLH$gen==3]
# Grandmother of idego
idgm <- IDmother(IDmother(idego))
# Number of grandmothers alive at birth of egos
ngch3 <- length(idego[Dd(idgm)>=Db(idego)])
# Proportion
round (100*ngch3/length(idego),2)

## -----------------------------------------------------------------------------
idego <- sample (dLH$ID[dLH$gen==3],1)

## -----------------------------------------------------------------------------
idgm <- IDmother(IDmother(idego))

## -----------------------------------------------------------------------------
alive85 <- Dd(idgm)>=Db(idgm)+85 

## -----------------------------------------------------------------------------
agegch85 <- NA
agegch85[alive85] <- (Db(idgm)+85-Db(idego))[alive85]

## -----------------------------------------------------------------------------
# IDs of members of third generation
idego <- dLH$ID[dLH$gen==3]
# IDs of their grandmothers
idgm <- IDmother(IDmother(idego))
# Is grandmother alive at 85?
alive <- Dd(idgm)>=Db(idgm)+85
# IDs of egos with living grandmother aged 85
idego85 <- idego[alive]

## -----------------------------------------------------------------------------
agegch85 <- Db(idgm[alive],dLH)+85-Db(idego85,dLH)

## -----------------------------------------------------------------------------
agesgmd <- Dd(unique(idgm))-Db(unique(idgm))

## -----------------------------------------------------------------------------
agesgchd <- Dd(IDmother(IDmother(idego)))-Db(idego)

## -----------------------------------------------------------------------------
iddb <-  unique(IDmother(idego85[agegch85<15]))

## -----------------------------------------------------------------------------
# IDs of mothers with double burden: iddb
# IDs of egos with mother who experiences double burden somewhere during her lifetime
idego_db <- IDch(iddb)
# Date of death of grandmother of ego with grandmother of 85+
dgmd <- Dd(IDmother(IDmother(idego_db)))
# 85th birthday of grandmother
dgm85 <- Db(IDmother(IDmother(idego_db))) + 85
# Date of birth of ego
dego0 <- Db(idego_db)
# 15th birthday of ego, assuming ego is alive at 15
dego15 <- Db(idego_db) + 15 
# Length of episodes during which a child experiences a mother with double burden
period_db <- pmin(dgmd,dego15) - pmax(dgm85,dego0) 
period_db[period_db<=0] <- NA
# Mean length of episode during which child has mother with double burden
mean(period_db,na.rm=TRUE)
# Number of children with a mother going through episode of double burden
length(period_db[!is.na(period_db)])
# Women who ever experience a double burden have a total number of children of: length(idego_db)
# Some of these children contribute to the bouble burden, other do not
# ID is childen who contribute to mother's double burden
idego_db2 <- idego_db[!is.na(period_db)]
# ID of children with mothers with double burden who do not contribute to double burden
idego_db3 <- idego_db[is.na(period_db)]
# IDs of women with double burden and IDs of children contributing to double burden
idmch_db <- cbind (IDmother(idego_db2),idego_db2)
# Number of children contributing to double burden for each women with double burden
z <- aggregate(idmch_db,list(IDmother(idego_db2)),function(x) length(x))
addmargins(table (z[,3]))
# IDs of women with highest number of children contributing to double burden
idm_max <- z$Group.1[z$V1==max(z$V1)]

## -----------------------------------------------------------------------------
# ID of individuals in generation 3 (children)
idego <- dLH$ID[dLH$gen==3]
# Ages of grandchildren five years prior to death of maternal grandmother
agegch5 <- Dd(IDmother(IDmother(idego)))-5-Db(idego)
# Is grandchild less than 15 five years prior to death of maternal grandmother?
chless15 <- agegch5<15 # Logical variable: true or false
# Number of grandchildren less than 15 five years prior to death of mother
table (chless15)
# The mothers of these grandchildren have some episode of double burden
idmAll <-  unique(IDmother(idego))
idmdb <- unique(IDmother(idego[chless15])) # 396 mothers with 621 children < 15
# Proportion of women in generation 2 with double burden
z <- round (100*length(idmdb)/length(idmAll),2)  

## -----------------------------------------------------------------------------
z <- Multistate(rates)$mslt$e0[,1]
round(z,2)

