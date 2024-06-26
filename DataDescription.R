library(mlbench)
library(dplyr)
library(randomForest)
library(locfit)
library(mgcv)
library(dplyr)
library(locfit)
library(dplyr)
library(splines)
library(lattice)
library(flexmix)
library(mgcv)
library(nlme)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(knitr)
library(caret)
library(rpart.plot)
library(randomForestSRC)
library(glmnet)

# 1.Data Description
## 1-1. Variables

This dataset contains 37 features, including `los`, `age`, `gender`, `ethnicity`, `religion`, `insurance`     `admission_type`, `admission_location`, `first_hosp_stay`, `diagnosis`, `los_reg_trs`, `aniongap_min`,    `aniongap_max`, `bicarbonate_min`, `bicarbonate_max`, `chloride_min`, `chloride_max`, `height`, `weight`, `hematocrit_min`, `hematocrit_max`, `hemoglobin_min`, `hemoglobin_max`, `platelet_min`, `platelet_max`, `bilirubin_max`, `sodium_max`, `potassium_max`, `heartrate_min`, `heartrate_max`, and `heartrate_mean`. 


model = lm(los~.,data=dat)
#summary(model)
name = c("ethnicity(black)", "religion", "aniongap_min", "bicarbonate_min","bicarbonate_max", "chloride_min", "chloride_max", "height", "weight", "heartrate_mean")
p.val = c(0.023371,0.022832,0.000696,0.012708,0.000209,6.46e-13,7.10e-08, 2e-16,8.14e-14,3.91e-05)
pval=data.frame(name,p.val)

kable(pval) %>% kable_styling(position = "center", latex_options = "HOLD_position")

## 1-2. New variables 

dat$aniongap = (dat$aniongap_min + dat$aniongap_max)/2
dat$bicarbonate = (dat$bicarbonate_min + dat$bicarbonate_max)/2
dat$chloride = (dat$chloride_min + dat$chloride_max)/2
dat$hematocrit = (dat$hematocrit_min + dat$hematocrit_max)/2
dat$hemoglobin = (dat$hemoglobin_min + dat$hemoglobin_max)/2
dat$platelet = (dat$platelet_min + dat$platelet_max)/2

# The new features `aniongap`, `bicarbonate`, `chloride`, `hematocrit`, `hemoglobin`, and `platelet` are added to dataset since the given data has both max and min of those features. 


## 1-3. Outliers
p1 = ggplot(data=dat, aes(x=weight, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + xlab('weight')+ylab('los')
p2 = ggplot(data=dat, aes(x=height, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('height')+ylab('los')
p3 = ggplot(data=dat, aes(x=aniongap, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('aniongap')+ylab('los')
p4 = ggplot(data=dat, aes(x=bicarbonate, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('bicarbonate')+ylab('los')
p5 = ggplot(data=dat, aes(x=chloride, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('chloride')+ylab('los')
p6 = ggplot(data=dat, aes(x=hematocrit, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('hematocrit')+ylab('los')
p7 = ggplot(data=dat, aes(x=sodium_max, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('sodium_max')+ylab('los')
p8 = ggplot(data=dat, aes(x=hemoglobin, y=los)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('hemoglobin')+ylab('los')

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
ggarrange(p5,p6,p7,p8,ncol=2,nrow=2)

# `height` was found to have a linear trend with `los`.
# `weight` was found to have a linear trend in certain weight divisions.
# `aniongap`, `bicarbonate`, `chloride`, `hematocrit`, `sodium_max`, and `hemoglobin` were hardly found to have a linear trend with `los`.
# Outliers are found from `dat` based on each scatter plot of `weight`, `height`, `aniongap`, `bicarbonate`, `chloride`, `hematocrit`, `sodium_max`, `heartrate_min`.


## 1-4. Variable Importance - Random Forest
fit = randomForest(los ~ ., 
                   data = dat, importance=TRUE)

imp=data.frame(importance(fit))
ord <- order(imp$X.IncMSE, decreasing=TRUE)
imp=imp[ord, ]
imp2 = imp[1:10,]

# Variable Importance
a=c("height", "weight", "heartrate_mean", "heartrate_min","admission_location", "hematocrit_min","hematocrit_max", "hematocrit","chloride_max", "chloride_min")
par(mfrow=c(1,2))
barplot(imp2$X.IncMSE,col="#69b3a2",ylim=c(0,50),ylab="X.IncMSE",names.arg=a,cex.names = 0.5,space=c(0.3,0.3,0.3,0.3,0.3),las=2,cex.axis=0.5)
barplot(imp2$IncNodePurity,col=rgb(0.8,0.1,0.1,0.6),ylim=c(0,350000),ylab="IncNodePurity",names.arg=a,cex.names = 0.5,space=c(0.3,0.3,0.3,0.3,0.3),las=2,cex.axis=0.5)

# The importance of each variable is measured by the percentage increase in the mean squared error (MSE) when that variable is removed from the model. The higher the percentage increase in MSE, the more important the variable is to the model.

# The two plots represent the IncreasedMSE and IncreasedNodepurity of each variable. According to the two plots, `height`, `weight`, `heartrate_mean`, are highly significant, while `admission_location`, `hematocrit`, `chloride_max`, `hematocrit_min`, `aniongap_min`, `hemoglobin_max` are weakly significant.


## 1-5. Interaction terms
cor_df = data.frame()
name = c('weight * height','height * heartrate_mean','chloride_min * sodium_max',
         'hematocrit * hemoglobin')
cor_ = c(cor(dat$weight,dat$height),cor(dat$height,dat$heartrate_mean),
         cor(dat$chloride_min,dat$sodium_max),cor(dat$hematocrit,dat$hemoglobin))
cor_df = data.frame(name,cor_)
colnames(cor_df) = c('Interactions','Correlation')

p9 = ggplot(data=dat, aes(x=height, y=weight)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('height')+ylab('weight')
p10 = ggplot(data=dat, aes(x=height, y=heartrate_mean)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('height')+ylab('heartrate_mean')
p11 = ggplot(data=dat, aes(x=chloride_min, y=sodium_max)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('chloride_min')+ylab('sodium_max')
p12 = ggplot(data=dat, aes(x=hematocrit, y=hemoglobin)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('hematocrit')+ylab('hemoglobin')

ggarrange(p9,p10,p11,p12,ncol=2,nrow=2)


kable(cor_df) %>% kable_styling(position = "center", latex_options = "HOLD_position")

# Correlations between variables are calculated, and scatter plots are drawn. According to the table, correlations between `weight&height`, `height&heartrate_mean`, `sodium_max&chloride_min`, and `hematocrit&hemoglobin` seem to be significant as the absolute values of correlations are significantly large. Based on scatter plots, `sodium_max&chloride_min` and `hematocrit&hemoglobin` were found to have a strong linear trend, while `height&heartrate_mean` has a relatively weak trend. 

# Therefore, they are considered to be included in the following models. 

## 1-6. Histogram of los
par(mfrow=c(1,1))
hist(dat$los,col='skyblue',ylim=c(0,700),xlim=c(0,200),main='Histogram of los',cex.lab = 0.7,cex.main = 1,cex.axis = 0.7,xlab='los')


#The feature `los` will be predicted by the following two models. As this project aims to predict `los` with high accuracy, it is crucial to examine the frequency of this feature. Therefore, the frequency of `los` is plotted, and it is found to be highly concentrated when `los` is less than 30. 