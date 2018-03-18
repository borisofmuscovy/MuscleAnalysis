#Used Packages
requirements = c("nlme", "effects", "pastecs", "lattice", "psych", "ggplot2", "GGally", "mice", "VIM", "aod", "BaM", "lme4", "ipw")
if (length(setdiff(requirements, rownames(installed.packages()))) > 0) {
  invisible(install.packages(setdiff(requirements, rownames(installed.packages())))) 
}
for (i in seq(1, length(requirements))) {
  invisible(library(requirements[i], character.only=T))
}

#Importing of the data
muscledata = read.table("muscle-incomplete.txt", header=T, na.strings = "NA")

#Explore the data + Descriptive Statistics
muscledata.summary = summary(muscledata)
muscledata.summary
plot(muscledata)
attach(muscledata)
muscledata_edit = na.omit(muscledata)

#For All Individuals
descrip.muscledata = stat.desc(muscledata_edit[,c("weight","calhour","calories")],basic = TRUE, desc = TRUE)
options(digits = 2)
descrip.muscledata
par(mfrow=c(1,3))
boxplot(weight, main='weight', col="pink")
boxplot(calhour, main='calhour', col="magenta")
boxplot(calories, main='calories', col="purple")
boxplot(calories~calhour, xlab="Calhour",ylab="Calories")
ggpairs(muscledata_edit)

#estimate correlation (individuals)
#scatterplot
par(mfrow=c(1,1))
plot(calories~calhour,  main='Calhour vs Calories')
abline(lm(calories~calhour), col=2)
ggplot(muscledata_edit, aes(x=calhour, y=calories))
+ geom_point(colour="red") 
+ geom_smooth(colour="orange", method="lm") 
+ ggtitle("calhour vs. calories")

#calculating the covariance and correlation
corr.calhour.calories = cor(calhour, calories)
corr.calhour.calories

#test the population correlation H0:correlation=0; H1:correlation!=0; 95%CI
corr.calhour.calories.test = cor.test(calhour, calories, alternative = "two.sided", method = "pearson")
corr.calhour.calories.test
corr.weight.calories.test = cor.test(weight, calories, alternative = "two.sided", method = "pearson")
corr.weight.calories.test

#Explore the missing data + descriptive statistics
muscledata_missing_aggr = aggr(muscledata, numbers = TRUE, prop = FALSE, ylab = c("Histogram of missing data", "Pattern"), col= c("pink", "purple"))
muscledata_missing_aggr

aggr(muscledata, combined=TRUE, numbers = TRUE, prop = TRUE, cex.numbers=0.87, varheight = FALSE, col= c("pink", "purple"))
#These two histograms show us that the missing calories bellong to the low calhour
#values but for weights the missing data is distributed evenly accross the spectra.
#This suggests MAR!!!!
histMiss(muscledata, col= c("pink", "purple"))
legend(40,8, legend=c("Observed Data", "Missing Data"),
       col=c("pink", "purple"), pch=19, cex=0.8)

histMiss(muscledata, pos=2, col= c("pink", "purple"))
legend(52,8, legend=c("Observed Data", "Missing Data"),
       col=c("pink", "purple"), pch=19, cex=0.8)

marginplot(muscledata[c("calhour","calories")], col= c("pink", "purple"))
legend(12, 345, legend=c("Observed Data", "Missing Data"),
       col=c("pink", "purple"), pch=19, cex=0.8)

marginplot(muscledata[c("weight","calories")], col= c("pink", "purple"))
legend(45, 345, legend=c("Observed Data", "Missing Data"),
       col=c("pink", "purple"), pch=19, cex=0.8)

##fitting the model
#Using stepwise with AIC to select the best model
muscledata.stepwise = step(lm(calories ~1, data=muscledata_edit), scope=~weight+calhour+weight*calhour, direction="both")

#calories = β0 + β1*weight + β2*calhour + β3*(weight*calhour) = 0
muscledata.complete.case = lm(calories~weight+calhour+weight*calhour, data=muscledata_edit)
muscledata.complete.case.summary = summary(muscledata.complete.case)
plot(allEffects(muscledata.complete.case))
muscledata.complete.case.summary

#handling missing data with MI(PMM)
muscledata.imp.pmm = mice(muscledata, meth = c("", "", "pmm"), m=100)
muscledata.fit.pmm = with(data=muscledata.imp, exp=glm(calories~weight+calhour+weight*calhour))
muscledata.pmm = pool(muscledata.fit.pmm)
summary(muscledata.pmm)
MI.fitted.values.pmm = complete(muscledata.imp.pmm, "long", inc=T)
muscledata.results.mi.pmm = glm(calories~weight+calhour+weight*calhour, data=MI.fitted.values.pmm)
dlist=list(calhour=seq(20,60,10))
plot(allEffects(muscledata.results.mi.pmm,xlevels=dlist)[1], main="PMM effects plot")

col = rep(c("pink","purple")[1+as.numeric(is.na(muscledata.imp.pmm$data$calories))],101)
stripplot(calories~.imp, data=MI.fitted.values.pmm, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number", main="Original data vs. generated data (PMM)")

#handling missing data with MI(norm)
muscledata.imp.norm = mice(muscledata, meth = c("", "", "norm"), m=100)
muscledata.fit.norm = with(data=muscledata.imp.norm, exp=glm(calories~weight+calhour+weight*calhour))
muscledata.norm = pool(muscledata.fit.norm)
summary(muscledata.norm)

MI.fitted.values.norm = complete(muscledata.imp.norm, "long", inc=T)
muscledata.results.mi.norm = glm(calories~weight+calhour+weight*calhour, data=MI.fitted.values.norm)
dlist=list(calhour=seq(20,60,10))
plot(allEffects(muscledata.results.mi.norm,xlevels=dlist)[1], main="NORM effects plot")

col = rep(c("pink","purple")[1+as.numeric(is.na(muscledata.imp.norm$data$calories))],101)
stripplot(calories~.imp, data=MI.fitted.values.norm, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number", main="Original data vs. generated data (NORM)")

##handling missing data with IPW
muscledata$r = as.numeric(!is.na(muscledata$calories))
muscledata.ipw.glm = lm(r ~ calhour, data=muscledata, family=binomial)
summary(muscledata.ipw.glm)
muscledata$w = 1/fitted(muscledata.ipw.glm)
muscledata.results.ipw= lm(calories~weight+calhour+weight*calhour, data=muscledata, weights=muscledata$w)
plot(allEffects(muscledata.results.ipw), main="IPW effects plot")
summary(muscledata.results.ipw)

## Likelihood ratio test null model versus full model
AIC(muscledata.complete.case)
AIC(muscledata.results.ipw)
anova(muscledata.fit.norm, muscledata.fit.pmm)

calories = c(complete(muscledata.imp.pmm)$calories, complete(muscledata.imp.norm)$calories)
method = rep(c("pmm", "norm"), each = nrow(muscledata))
calm = data.frame(muscledata = calories, method = method)
histogram( ~calories | method, data = calm, nint = 25)

muscledata.imp.pmm = mice(muscledata, meth = c("", "", "pmm"), m=100)
muscledata.fit.pmm = with(data=muscledata.imp, exp=lm(calories~weight+calhour+weight*calhour))

muscledata.imp.norm = mice(muscledata, meth = c("", "", "norm"), m=100)
muscledata.fit.norm = with(data=muscledata.imp, exp=lm(calories~weight+calhour+weight*calhour))
pool.r.squared(muscledata.fit.norm)
pool.r.squared(muscledata.fit.pmm)
summary(muscledata.results.ipw)         
summary(muscledata.complete.case)   

# ROBINS NON-13 TESTS

muscledata = read.table("muscle-incomplete.txt", header=T, na.strings = "NA")
muscledata.without13 = muscledata[!muscledata$calhour == 13,]
muscledata.without13.imp <- mice(muscledata.without13, meth = c("", "", "norm"), m=100) # imputation of different values, 100 different complete datasets
muscledata.without13.fit <- with(data=muscledata.without13.imp, exp=glm(calories~weight+calhour+weight*calhour)) # analysis, creating a Q for each imputed dataset
muscledata.without13.est <- pool(muscledata.without13.fit) # pooling the Qs together to create one estimate Q mean. if Q are approx normally distributed, we calculate mean over all Q and sum the within and between imputation variance using Rubins method
summary(muscledata.without13.est)

MI.fitted.values.pmm.excl.13 = complete(muscledata.without13.est, "long", inc=T)
muscledata.results.mi.pmm.excl.13 = glm(calories~weight+calhour+weight*calhour, data=MI.fitted.values.pmm.excl.13)
dlist=list(calhour=seq(20,60,10))
plot(allEffects(MI.fitted.values.pmm.excl.13,xlevels=dlist)[1], main="PMM effects plot")

col <- rep(c("pink","purple")[1+as.numeric(is.na(muscledata.imp$data$calories))],101)
stripplot(calories~.imp, data=MI.fitted.values, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number")
