###################################################
### chunk number 1: 
###################################################
options(width=70)


###################################################
### chunk number 2: 
###################################################
library(mitools)
data.dir<-system.file("dta",package="mitools")

## read in data
library(foreign)
women<-imputationList(lapply(list.files(data.dir,
				pattern="f.\\.dta",full=TRUE),
                           read.dta))
men<-imputationList(lapply(list.files(data.dir,
				pattern="m.\\.dta",full=TRUE),
                           read.dta))


###################################################
### chunk number 3: 
###################################################
## add sex variable
women<-update(women,sex=0)
men<-update(men, sex=1)
## combine two sets of imputations
all<-rbind(women,men)
all
colnames(all)


###################################################
### chunk number 4: 
###################################################
with(all, table(sex, drkfre))


###################################################
### chunk number 5: 
###################################################
all<-update(all, drkreg=as.numeric(drkfre)>2)
## tables
with(all, table(sex, drkreg))


###################################################
### chunk number 6: 
###################################################
## logistic regression model
model1<-with(all, glm(drkreg~wave*sex, family=binomial()))
MIcombine(model1)
summary(MIcombine(model1))


###################################################
### chunk number 7: 
###################################################
beta<-MIextract(model1, fun=coef)
vars<-MIextract(model1, fun=vcov)
summary(MIcombine(beta,vars))


