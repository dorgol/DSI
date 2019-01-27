# Jane's modifications Jan 2019

debugonce(esem)
options(error = recover) #turn off
options(error = NULL) #turn on 

library(foreign)
AMPD <- read.spss("AMPD scored.sav", to.data.frame=TRUE, use.value.labels=T)
str(AMPD) #shows that some variable are factors
AMPD <- AMPD[,sapply(AMPD, is.numeric)] #removes the factors -- index for varsX below is shifted accordingly
m1 <-esem(AMPD, varsX = 205:229, varsY = NULL, nfX = 5, nfY = NULL) #fixed the earlier error, has a new error due to no y vars.
fa(AMPD[,205:229], nfactors = 5) #a function for exploratory factor analysis -- possibly closer to the goal at this point?
####load the data 

library(foreign)
AMPD <- read.spss("AMPD scored.sav", to.data.frame=TRUE, use.value.labels=T)

#try with esem function again ----
library(psych)
#m1 <-esem(AMPD, varsX = 213:237, varsY = NULL, nfX = 5, nfY = NULL)

esem.diagram(esem=m1,labels=NULL,cut=.3,errors=FALSE,simple=TRUE,
             regression=FALSE,lr=TRUE, digits=1,e.size=.1,adj=2,
             main="Exploratory Structural Model")
#this is not working



#check that all variables are numeric
sapply(AMPD[15:14, 213:237], is.numeric)


sapply(AMPD, class)
#they all appear to be numeric

#### New attempt from paper ----
#Step one --> identify the data (FFM data for first esem)
names(AMPD)

#FFM
head(AMPD[,c(15:44)])

#PID-5
head(AMPD[,c(213:237)])

#Step two --> create a correlation matrix
#load the cormat funtion


RFFM1 <- cor(AMPD[,c(15:44)])
is.matrix(RFFM1)

# not working with the matrix, Maybe you just need the lower triangle and the diagonal?
RFFM1[upper.tri(RFFM1,diag=F)] <- 0 

View(RFFM1)

#tried another way of getting a matrix to see if that would work 

#load in funtion
source("http://www.sthda.com/upload/rquery_cormat.r")

#creat Matrix
RFFM<-rquery.cormat(AMPD[,c(15:44)])

#extract matrix
RFFM1 <- RFFM$r

#factanul isn't working because the variables aren't numeric
class(RFFM1)
sapply(RFFM1, is.numeric) 
sapply(RFFM1, class)

View(RFFM1)

#is it b/c it is a dataframe? 
as.matrix(RFFM1)


#is it because the uper trianle is empty? 
RFFM1[upper.tri(RFFM1,diag=F)] <- 0 

#now upper triagle is all na
RFFM1[is.na(RFFM1)]<-0
summary(warnings())
#this didn't work to fix it. 

sapply(RFFM1, class)



#step 2 -> get factor loading matrix


RFFM11 <- factanal(RFFM1, factors = 5)

det(RFFM1)

#this doesn't work when I put the correlation matrix directly in, so I'm going to try it with the data matrix

RFFM11 <- factanal(AMPD[,c(15:44)], factors = 5)
F <- matrix(RFFM1$loadings[1:150],30,5)

#This also isn't working, the factor analysis worked, but not extracting the factors. 







#### Tried Mirt package 1/24/19----
#load packages
if(!require("mirt")) install.packages("mirt")
library(mirt)

#create model object

FFM <- mirt(data = AMPD[,c(15:44)], model = 5, method = "QMCEM")

summary (FFM, rotate = "geominT")

itemfit(FFM)




#####old work ----
install.packages("sem")
library(sem)

Cor.FFMRF <- data.matrix(cor(AMPD[,c(15:44)]))


Cor.FFMRF[lower.tri(Cor.FFMRF)] <- 0

sapply(R.FFMRF1, class)


R.FFMRF11 <- factanal(Cor.FFMRF, factors = 5)






##### input for the esem

#correlation matrix for ffmrf items (variables 15-44 in the data set), ocean variables were 395-399
names(AMPD) #to identify the variables
names(AMPD[,c(15:44,395:399)]) #check column numbers

Cor.FFMRF <- data.matrix(cor(AMPD[,c(15:44)]))

install.packages("Deducer")
library(Deducer)
Cor.FFMRF <- cor.matrix(AMPD[,c(15:44)])

install.packages("GPArotation")

sapply(Cor.FFMRF, class)


#r=Cor.FFMRF
#varsX=AMPD[,15:44]
#fnx=5
#n.obs=506
nrow(AMPD)
#fm=pa #this is principle axis factor rotation #might need to change this
#rotate=geominQ
#leave the rest on default

library(psych)


library(GPArotation)



model1 <-esem(r=cor(AMPD[,c(15:44)]), varsX=1:15, varsY = 16:30, nfX = 3, nfY = 2, n.obs = 506, fm = "pa", 
     rotate = "geominQ", plot = TRUE, cor = "cor", use = "pairwise",weight=NULL)
?esem


esem.diagram(esem=model1,labels=names(Cor.FFMRF),cut=.3,errors=FALSE,simple=TRUE,
             regression=FALSE,lr=TRUE, digits=1,e.size=.1,adj=2,
             main="Exploratory Structural Model")

interbattery(r, varsX, varsY, nfX = 1, nfY = 1, n.obs = NULL,cor = "cor", 
             use = "pairwise",weight=NULL)