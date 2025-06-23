# Marek Let --- University of South Bohemia in Ceske Budejovice (USB)

### Instal packages

install.packages(lcmm)
install.packages(randtoolbox)
install.packages(mgcv)

### Data import - import data using clipboard from Das_et_al_2025_dataset.xlsx

## load from lists called Exp_1, Exp_2, Exp_3_food_zone, and Exp3_shelter individually !!

df1<-read.delim2("clipboard",as.is = FALSE)
summary(df1)

## Make unique and valid animal ID across data

df1$Animal.ID<-as.factor(df1$Animal.ID)

df1<-transform(df1,NewID = interaction(Animal.ID,group))

## Transform data

# Exp_1
df1<-transform(df1,logDistance.moved=log(Distance.moved+1),logVelocity= log(Velocity+1),logActivity = (log((Activity..s/3600)+1)))

# Exp_2
df1<-transform(df1,logDistance.moved=log(Distance_moved_cm+1),logVelocity= log(Velocity_cm.s+1),logActivity = log((Activity_s/3600)+1),logShelter = log((shelter_s/3600)+1))

# Exp_3_food_zone
df1<-transform(df1,logTime = log((Time/3600)+1))

# Exp_3_shelter
df1<-transform(df1,logTime = log((Time/3600)+1))


### fit lclmms



library(lcmm)
library(randtoolbox)

df1$NewID<-as.numeric(df1$NewID) # Necessary to adjust as numerical value
summary(df1)


# Select required response variable - here logDistance.moved used

model_lcmm_1 <- hlme(fixed = logDistance.moved ~ poly(hour, 2), random = ~ 1,subject = "NewID", ng = 1,data = df1)

model_lcmm_2 <- hlme(fixed = logDistance.moved ~ poly(hour, 2), mixture = ~ poly(hour, 2), random = ~ 1, subject = "NewID",ng = 2,data = df1, nwg = TRUE,B = random(model_lcmm_1))

model_lcmm_3 <- hlme(fixed = logDistance.moved ~ poly(hour, 2), mixture = ~ poly(hour, 2), random = ~ 1,subject = "NewID", ng = 3, data = df1,nwg = TRUE,B = random(model_lcmm_1))

model_lcmm_4 <- hlme(fixed = logDistance.moved ~ poly(hour, 2), mixture = ~ poly(hour, 2), random = ~ 1,subject = "NewID", ng = 4, data = df1,nwg = TRUE,B = random(model_lcmm_1))

model_lcmm_5 <- hlme(fixed = logDistance.moved ~ poly(hour, 2), mixture = ~ poly(hour, 2), random = ~ 1,subject = "NewID", ng = 5, data = df1,nwg = TRUE,B = random(model_lcmm_1))

## Compare models - please note that results can always slightly differ when the process is repeated

summarytable(model_lcmm_1, model_lcmm_2,model_lcmm_3,model_lcmm_4, model_lcmm_5)

# Plot selected model - here model_lcmm_2 plotted
plot(model_lcmm_2, which = "fit", var.time = "hour")


## Add latent class identity into data - here model_lcmm_2 selected and used

df1 <- merge(df1, model_lcmm_2$pprob[, c("NewID", "class")], by = "NewID")


### Fit GA(M)Ms

library(mgcv)

df1$class<-as.factor(df1$class) # Necessary to adjust as factor


## Comparison of GAM with GAMM by AIC

# Select required response variable - here logDistance.moved used

GAM.0<-gam(logDistance.moved~1,data = df1, method = "REML")
GAMM.0<-gamm(logDistance.moved~1,random = list(NewID=~1),data = df1, method = "REML")

AIC(GAM.0,GAMM.0$lme)

## Fit GAMMs

# Select required response variable - here logDistance.moved used

GAMM.3c1.0<-gamm(logDistance.moved~s(hour, bs = "cc"),random = list(NewID=~1),data = df1, method = "ML")
GAMM.3c1.1<-gamm(logDistance.moved~s(hour, bs = "cc")+s(hour,by = class, bs = "cc"),random = list(NewID=~1),data = df1, method = "ML")
GAMM.3c1<-gamm(logDistance.moved~s(hour, bs = "cc")+s(hour,by = class, bs = "cc")+group,random = list(NewID=~1),data = df1, method = "ML")
GAMM.3c1.2<-gamm(logDistance.moved~s(hour, bs = "cc")+s(hour,by = class, bs = "cc")+s(hour,by = group, bs = "cc"),random = list(NewID=~1),data = df1, method = "ML")
GAMM.3c1.3<-gamm(logDistance.moved~s(hour, bs = "cc")+s(hour,by = class, bs = "cc")+s(hour,by = group, bs = "cc")+group,random = list(NewID=~1),data = df1, method = "ML")

## Comparison of GAMMs by AIC

AIC(GAMM.3c1.0$lme,GAMM.3c1.1$lme,GAMM.3c1$lme,GAMM.3c1.2$lme,GAMM.3c1.3$lme)

## LRTs

anova(GAMM.3c1.0$lme,GAMM.3c1.1$lme)
anova(GAMM.3c1.1$lme,GAMM.3c1$lme)
anova(GAMM.3c1$lme,GAMM.3c1.2$lme)
anova(GAMM.3c1.1$lme,GAMM.3c1.2$lme)
anova(GAMM.3c1.2$lme,GAMM.3c1.3$lme)

anova(GAMM.3c1$lme,GAMM.3c1.3$lme)
anova(GAMM.3c1.0$lme,GAMM.3c1$lme)

## Visual check of regression diagnostics - here GAMM.3c1 selected and used

scatter.smooth(sqrt(abs(GAMM.3c1$gam$residuals))~fitted(GAMM.3c1$gam))

res<-resid(GAMM.3c1$gam)
hist(res)

### Plot models, e.g using tidymv and ggplot2 libraries





