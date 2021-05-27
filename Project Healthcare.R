setwd(choose.dir())
getwd()
install.packages("readxl")
library(readxl)
HealthDetails <- read_excel("healthcare.xlsx")
View(HealthDetails)

hist(HealthDetails$AGE,main = "Frequency of patients",col = "blue",xlab ="Age")

attach(HealthDetails)
AGE=as.factor(AGE)
summary(AGE)

aggregate(TOTCHG~AGE,FUN=sum,data = HealthDetails)
max(aggregate(TOTCHG~AGE,FUN=sum,data = HealthDetails))

#2
hist(APRDRG,col = "cyan1",main = "Frequency of the Treatments",xlab = "Treatment Categories")
APRDRG_factor<-as.factor(HealthDetails$APRDRG)
summary(APRDRG_factor)

which.max(summary(APRDRG_factor))

agg=aggregate(TOTCHG~APRDRG,FUN = sum,data=HealthDetails)
agg

agg[which.max(agg$TOTCHG),]

#3
HealthDetails=na.omit(HealthDetails)
HealthDetails$RACE=as.factor(HealthDetails$RACE)
model=aov(TOTCHG~RACE,data = HealthDetails)
model

summary(model)

summary(HealthDetails$RACE)

#4
HealthDetails$FEMALE=as.factor(HealthDetails$FEMALE)
regression=lm(TOTCHG~AGE+FEMALE,data = HealthDetails)
summary(regression)
summary(HealthDetails$FEMALE)


#5
HealthDetails$RACE=as.factor(HealthDetails$RACE)
model_linear=lm(LOS~AGE+FEMALE+RACE,data = HealthDetails)
summary(model_linear)

#6
model_cost=lm(TOTCHG~.,data = HealthDetails)
summary(model_cost)
