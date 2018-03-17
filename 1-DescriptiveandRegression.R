#install.packages("nlme") if you don't have it already
library(nlme)
library(lme4)
library(effects)
library(lattice)
library(pastecs)
library(psych)
library(ggplot2)
library(GGally)
library(mice)
library(VIM)
library(aod)
library(BaM)
muscledata = read.table("muscle-incomplete.txt", header=T, na.strings = "NA")
muscleSum = summary(muscledata)
plot(muscledata)
muscleSum
##From the brief-intro pdf, the data are collected from 24 volunteers.
#1. examine them individually
#2. group them by weight and examine

#missing data
muscledata_missing_aggr = aggr(muscledata, numbers = TRUE, prop = FALSE, ylab = c("Histogram of missing data", "Pattern"))
muscledata_missing_aggr

aggr(muscledata, combined=TRUE, numbers = TRUE, prop = TRUE, cex.numbers=0.87, varheight = FALSE)
barMiss(muscledata[,c("calories","weight")])
histMiss(muscledata)
histMiss(muscledata, pos=2)

marginplot(muscledata[c("calhour","calories")])
marginplot(muscledata[c("weight","calories")])

#creating column to assign ID's to the different individuals (same weight = same individual).
#having multiple rows belonging to 1 individual generally signals we should use ID as a random factor, see lesson 3/4.
#ID = c(rep(1,3),rep(2,4),rep(3,4),rep(4,2),rep(5,3),rep(6,5),rep(7,3))
#muscledata = cbind(ID, muscledata)

#we have a lesson on missing data so I'm guessing this will be more complex than just leaving it out.
#doing this so we can do some preliminary work.
muscledata_edit = na.omit(muscledata)
calories = muscledata_edit$calories
calhour = muscledata_edit$calhour
weight = muscledata_edit$weight
## Descriptive Statistics
#for all individuals
descrip.muscledata = stat.desc(muscledata_edit[,c("weight","calhour","calories")],basic = TRUE, desc = TRUE)
options(digits = 2)
descrip.muscledata
par(mfrow=c(1,3))
boxplot(weight, main='weight', col=2)
boxplot(calhour, main='calhour', col=3)
boxplot(calories, main='calories', col=4)
boxplot(calories~calhour, xlab="Calhour",ylab="Calories")
ggpairs(muscledata_edit)

#group them by weight
describeBy(muscledata_edit[c("calhour","calories")], group = weight)


#estimate correlation(individuals)
#scatterplot
par(mfrow=c(1,1))
plot(calories~calhour,  main='Calhour vs Calories')
abline(lm(calories~calhour), col=2)
ggplot(muscledata_edit, aes(x=calhour, y=calories))+ geom_point(colour="blue") + geom_smooth(colour="black", method="lm")

#calculating the covariance and correlation
cov.calhour.calories = cov(calhour, calories)
corr.calhour.calories = cor(calhour, calories)
cov.calhour.calories
corr.calhour.calories
#test the population correlation H0:correlation=0; H1:correlation!=0; 95%CI
corr.calhour.calories.test = cor.test(calhour, calories, alternative = "two.sided", method = "pearson")
corr.calhour.calories.test
#result: p-value = 2e-08 reject H0. correlation existed!!!
#fitting the model
#calories = β0 + β1*weight + β2*calhour + β3*(weight*calhour) =0
muscledata.complete.case = lm(calories~weight+calhour+weight*calhour, data=muscledata_edit)
muscledata.complete.case.summary = summary(muscledata.complete.case)
plot(allEffects(muscledata.complete.case))
muscledata.complete.case.summary

#handling missing data with MI
muscledata.imp <- mice(muscledata, meth = c("", "", "pmm"), m=100)
muscledata.fit <- with(data=muscledata.imp, exp=glm(calories~weight+calhour+weight*calhour))
MI.matrix<-matrix(0,100,4)
for(k in 1:100){ MI.matrix[k,]<-coefficients(muscledata.fit$analyses[[k]])}
MI.results=data.frame(Intercept=MI.matrix[,1], weight=MI.matrix[,2],calhour=MI.matrix[,3], interaction=MI.matrix[,4])
MI.results[1:10,]
muscledata.est <- pool(muscledata.fit)
summary(muscledata.est)

MI.fitted.values = complete(muscledata.imp, "long", inc=T)
muscledata.results.MIALL <- glm(calories~weight+calhour+weight*calhour, data=MI.fitted.values)
dlist=list(calhour=seq(20,60,10))
plot(allEffects(muscledata.results.MIALL,xlevels=dlist)[1])

col <- rep(c("pink","purple")[1+as.numeric(is.na(muscledata.imp$data$calories))],101)
stripplot(calories~.imp, data=MI.fitted.values, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number")

##handling missing data with IPW
muscledata$r<-as.numeric(!is.na(muscledata$calories))
head(muscledata)
muscledata.ipw.glm<-glm(r ~ weight+calhour+weight*calhour, data=muscledata,family=binomial)
summary(muscledata.ipw.glm)
muscledata$w<-1/fitted(muscledata.ipw.glm)
head(muscledata)
muscledata.results.ipw<- glm(calories~weight+calhour+weight*calhour, data=muscledata, weights=muscledata$w)
plot(allEffects(muscledata.results.ipw))
summary(muscledata.results.ipw)


## Likelihood ratio test null model versus full model
muscledata.complete.case.int = lm(calories~1, data=muscledata_edit) 
anova(muscledata.complete.case.int,muscledata.complete.case)
#H0 :β1 =β2 =β3 =0
## Sequential building of the model
muscledata.anova = anova(muscledata.complete.case)
muscledata.anova
muscledata.complete.case2 = lm(calories~calhour+weight+weight*calhour, data=muscledata_edit)
muscledata.anova2 = anova(muscledata.complete.case2)
muscledata.anova2
muscledata.complete.case.final = lm(calories~calhour+weight+weight*calhour, data=muscledata_edit)
muscledata.final.summary = summary(muscledata.complete.case.final)
muscledata.final.summary
#(calories)i = -330.884 + 11.787*(calhour)i + 7.728*(weight)i - 0.132*(calhour*weight)i + εi

