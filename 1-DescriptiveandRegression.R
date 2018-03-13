#install.packages("nlme") if you don't have it already
install.packages("nlme")
install.packages("pastecs")
install.packages("effects")
library(nlme)
library(lme4)
library(effects)
library(lattice)
library(pastecs)
library(psych)
library(ggplot2)
library(GGally)
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)
install.packages("aod")
library(aod)
install.packages("BaM")
library(BaM)
muscledata = read.table("muscle-incomplete.txt", header=T, na.strings = "NA")
muscleSum = summary(muscledata)
muscleSum
##From the brief-intro pdf, the data are collected from 24 volunteers.
#1. examine them individually
#2. group them by weight and examine


#missing data
muscledata_missing_aggr = aggr(muscledata, numbers = TRUE, prop = FALSE, ylab = c("Histogram of missing data", "Pattern"))
muscledata_missing_aggr


#creating column to assign ID's to the different individuals (same weight = same individual).
#having multiple rows belonging to 1 individual generally signals we should use ID as a random factor, see lesson 3/4.
#ID = c(rep(1,3),rep(2,4),rep(3,4),rep(4,2),rep(5,3),rep(6,5),rep(7,3))
#muscledata = cbind(ID, muscledata)

#we have a lesson on missing data so I'm guessing this will be more complex than just leaving it out.
#doing this so we can do some preliminary work.
muscledata_edit = na.omit(muscledata)

ID = c(rep(1,3),rep(2,4),rep(3,4),rep(4,2),rep(5,3),rep(6,5),rep(7,3))
muscledata_weight = cbind(ID, muscledata)
group1 = subset(muscledata_weight, ID=="1")
group2 = subset(muscledata_weight, ID=="2")
group3 = subset(muscledata_weight, ID=="3")
group4 = subset(muscledata_weight, ID=="4")
group5 = subset(muscledata_weight, ID=="5")
group6 = subset(muscledata_weight, ID=="6")
group7 = subset(muscledata_weight, ID=="7")


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
muscledata.lm = lm(calories~weight+calhour+weight*calhour, data=muscledata_edit)
muscledata.lm.summary = summary(muscledata.lm)
muscledata.lm.summary

##handling missing data with IPW
muscledata$r<-as.numeric(!is.na(muscledata$calories))
head(muscledata)
muscledata.ipw.glm<-glm(r ~ weight+calhour+weight*calhour, data=muscledata,family=binomial)
summary(muscledata.ipw.glm)
muscledata$w<-1/fitted(muscledata.ipw.glm)
head(muscledata)
muscledata.results.ipw<- glm(calories~weight+calhour+weight*calhour, data=muscledata, weights=muscledata$w)
summary(muscledata.results.ipw)


#handling missing data with MI
muscledata.imp <- mice(muscledata, meth = c("", "", "pmm"), m=100)
muscledata.fit <- with(data=muscledata.imp, exp=glm(calories~weight+calhour+weight*calhour))
MI.matrix<-matrix(0,100,4)
for(k in 1:100){ MI.matrix[k,]<-coefficients(muscledata.fit$analyses[[k]])}
MI.results=data.frame(Intercept=MI.matrix[,1], weight=MI.matrix[,2],calhour=MI.matrix[,3], interaction=MI.matrix[,4])
MI.results[1:10,]
muscledata.est <- pool(muscledata.fit)
summary(muscledata.est)



## Likelihood ratio test null model versus full model
muscledata.lm.int = lm(calories~1, data=muscledata_edit) 
anova(muscledata.lm.int,muscledata.lm)
#H0 :β1 =β2 =β3 =0
## Sequential building of the model
muscledata.anova = anova(muscledata.lm)
muscledata.anova
muscledata.lm2 = lm(calories~calhour+weight+weight*calhour, data=muscledata_edit)
muscledata.anova2 = anova(muscledata.lm2)
muscledata.anova2
muscledata.lm.final = lm(calories~calhour+weight+weight*calhour, data=muscledata_edit)
muscledata.final.summary = summary(muscledata.lm.final)
muscledata.final.summary
#(calories)i = -330.884 + 11.787*(calhour)i + 7.728*(weight)i - 0.132*(calhour*weight)i + εi



#？？？？
#estimate correlation(in group)
xyplot(calories~calhour|factor(weight))
panel.regression = function(x,y){
  panel.xyplot(x,y) 
  panel.abline(lm(y~x))}
xyplot(calories~calhour|factor(weight), panel = panel.regression)
#group 6: weight=62
#the only group with enough data to do a correlation test
#calculating the covariance and correlation
cov.calhour.calories_group6 = cov(group6$calhour, group6$calories)
corr.calhour.calories_group6 = cor(group6$calhour, group6$calories)
cov.calhour.calories_group6
corr.calhour.calories_group6
#test the population correlation H0:correlation=0; H1:correlation!=0; 95%CI
corr.calhour.calories_group6.test = cor.test(group6$calhour, group6$calories, alternative = "two.sided", method = "pearson")
corr.calhour.calories_group6.test
#result: p-value = 0.003 reject H0. correlation existed!!!


#I saw random factors in my previous stats course so I'm just going to plop down some code here with ID as a random factor.
#not sure if we will use the same methods in this course.
#by setting ID as a random factor, we're basicly telling the model to
#   1. pool all observations belonging to the same ID (since there might be additional correlation going on between those values)
#   2. assume that the 7 IDs observed here are just a subset of a larger population (-> the population of all humans)
xyplot(calories~calhour,data=muscledata, groups = weight, type="o")
lme = lme(calories ~ calhour + weight, random=~1|ID, correlation=corCompSymm(form=~1|ID), data=muscledata)
anova(lme)
summary(lme)
plot(allEffects(mod=lme))

#alternative, don't realy think we need the code below but it's another way of doing it; you can do more complex stuff with this.
lmer = lmer(calories ~ calhour + weight + (1|ID), data=muscledata)
plot(allEffects(mod=lmer))
AIC(lme, lmer) #AIC is lower here because we don't make the extra assumption of compound symmetry would be my guess.
