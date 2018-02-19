#install.packages("nlme") if you don't have it already
library(nlme)
library(lme4)
library(effects)
library(lattice)

muscledata = read.table("muscle-incomplete.txt", header=T, na.strings = "NA")
muscleSum = summary(muscledata)

#creating column to assign ID's to the different individuals (same weight = same individual).
#having multiple rows belonging to 1 individual generally signals we should use ID as a random factor, see lesson 3/4.
ID = c(rep(1,3),rep(2,4),rep(3,4),rep(4,2),rep(5,3),rep(6,5),rep(7,3))
muscledata = cbind(ID, muscledata)

#we have a lesson on missing data so I'm guessing this will be more complex than just leaving it out.
#doing this so we can do some preliminary work.
muscledata = na.omit(muscledata)

#I saw random factors in my previous stats course so I'm just going to plop down some code here with ID as a random factor.
#not sure if we will use the same methods in this course.
#by setting ID as a random factor, we're basicly telling the model to
#   1. pool all observations belonging to the same ID (since there might be additional correlation going on between those values)
#   2. assume that the 7 IDs observed here are just a subset of a larger population (-> the population of all humans)
xyplot(calories ~ calhour,data=muscledata,group=ID,type="o")
lme = lme(calories ~ calhour + weight, random=~1|ID, correlation=corCompSymm(form=~1|ID), data=muscledata)
anova(lme)
summary(lme)
plot(allEffects(mod=lme))

#alternative, don't realy think we need the code below but it's another way of doing it; you can do more complex stuff with this.
lmer = lmer(calories ~ calhour + weight + (1|ID), data=muscledata)
plot(allEffects(mod=lmer))
AIC(lme, lmer) #AIC is lower here because we don't make the extra assumption of compound symmetry would be my guess.
