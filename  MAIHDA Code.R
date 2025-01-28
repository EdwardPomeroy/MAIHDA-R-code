#clearing memory 
rm(list=ls())

#setting working directory 

getwd()

setwd("/Users/edwardpomeroy/Desktop/")

# load packages ####
install.packages("haven", "tidyverse", "ggeffects", "merTools", "labelled", 
                 "sjPlot", "Metrics")
library(haven)
library(tidyverse)
library(ggeffects)
library(lme4)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)

# Loading in my data ####

mydata<- read.csv("Variables_NEW.csv")

# making variables numeric ####

mydata$care_outside<-as.numeric(mydata$care_outside)
is.numeric(mydata$care_outside)

mydata$sex<-as.numeric(mydata$sex)
is.numeric(mydata$sex)

mydata$children_HH<-as.numeric(mydata$children_HH)
is.numeric(mydata$children_HH)

mydata$cohabit<-as.numeric(mydata$cohabit)
is.numeric(mydata$cohabit)

mydata$care_inside<-as.numeric(mydata$care_inside)
is.numeric(mydata$care_inside)

mydata$new_employment<-as.numeric(mydata$new_employment)
is.numeric(mydata$new_employment)

mydata$income_INDI<-as.numeric(mydata$income_INDI)
is.numeric(mydata$income_INDI)

mydata$age_con<-as.numeric(mydata$age_con)
is.numeric(mydata$age_con)

table(mydata$age_con)

library(dplyr)

mydata_MAIDHA1<-select(mydata, care_outside, sex, children_HH, cohabit, care_inside, new_employment, income_INDI, age_con)

# generate stratum id 

mydata_MAIDHA1$stratum <- 100000*mydata_MAIDHA1$sex + 10000*mydata_MAIDHA1$children_HH + 1000*mydata_MAIDHA1$care_inside+ 100*mydata_MAIDHA1$cohabit+
  10*mydata_MAIDHA1$new_employment+ 1*mydata_MAIDHA1$income_INDI

View(mydata_MAIDHA1)

# Turn the stratum identifier into a factor variable

is.factor(mydata_MAIDHA1$stratum)

mydata_MAIDHA1$stratum <- as.factor(mydata_MAIDHA1$stratum)

is.factor(mydata_MAIDHA1$stratum)

# sort data by stratum
# this has ordered them by stratum (11111 first)
mydata_MAIDHA1 <- mydata_MAIDHA1[order(mydata_MAIDHA1$stratum),]

View(mydata_MAIDHA1)

# tabulate stratum

table(mydata_MAIDHA1$stratum)

# Generate a new variable which records stratum size

mydata_MAIDHA1 <- mydata_MAIDHA1 %>%
  group_by(stratum) %>%
  mutate(strataN = n())

table(mydata_MAIDHA1$stratum)

# Collapse the data down to a stratum-level dataset 

stratum_level2 <- aggregate(x=mydata_MAIDHA1[c("care_outside")], 
                            by=mydata_MAIDHA1[c("sex", "children_HH","care_inside","cohabit","new_employment","income_INDI",
                                                "stratum", "strataN")],
                            FUN=mean)

# convert the outcome from a proportion to a percentage
stratum_level2$care_outside <- stratum_level2$care_outside*100

View(stratum_level2)

# Testing sample strata size

# Get the counts for each stratum
stratum_counts <- table(mydata_MAIDHA1$stratum)

# Filter strata that have between 1 and 9 individuals
small_strata <- stratum_counts[stratum_counts >= 1 & stratum_counts <= 9]

# Display this and count number 
small_strata

# Count the number of strata
num_strata <- length(small_strata)

# Display the count
print(num_strata)

# Filter strata that have between 10 and 29 individuals
small_strata1 <- stratum_counts[stratum_counts >= 10 & stratum_counts <= 29]

# Display this and count number 
small_strata1

# Count the number of strata
num_strata1 <- length(small_strata1)

# Display the count
print(num_strata1)

# Filter strata that have between 30 and 49 individuals
small_strata2 <- stratum_counts[stratum_counts >= 30 & stratum_counts <= 49]

# Display this and count number 
small_strata2

# Count the number of strata
num_strata2 <- length(small_strata2)

# Display the count
print(num_strata2)

# Filter strata that have between 50 and 99 individuals
small_strata3 <- stratum_counts[stratum_counts >= 50 & stratum_counts <= 99]

# Display this and count number 
small_strata3

# Count the number of strata
num_strata3 <- length(small_strata3)

# Display the count
print(num_strata3)

# Filter strata that have between 50 and 99 individuals
small_strata4 <- stratum_counts[stratum_counts >= 100]

# Display this and count number 
small_strata4

# Count the number of strata
num_strata4 <- length(small_strata4)

# Display the count
print(num_strata4)

# run MADIHA ####

# sex

table(mydata_MAIDHA1$sex)

mydata_MAIDHA1$sex[mydata_MAIDHA1$sex==1] <-"Male"
mydata_MAIDHA1$sex[mydata_MAIDHA1$sex==2] <- "Female"

table(mydata_MAIDHA1$sex) 

is.factor(mydata_MAIDHA1$sex)
mydata_MAIDHA1$sex<-as.factor(mydata_MAIDHA1$sex)
is.factor(mydata_MAIDHA1$sex)

mydata_MAIDHA1$sex<- factor(mydata_MAIDHA1$sex, levels=c("Male", "Female"))

table(mydata_MAIDHA1$sex)   

# children_HH

table(mydata_MAIDHA1$children_HH)

mydata_MAIDHA1$children_HH[mydata_MAIDHA1$children_HH==1] <-"None"
mydata_MAIDHA1$children_HH[mydata_MAIDHA1$children_HH==2] <-"At least one"

is.factor(mydata_MAIDHA1$children_HH)
mydata_MAIDHA1$children_HH<-as.factor(mydata_MAIDHA1$children_HH)
is.factor(mydata_MAIDHA1$children_HH)

mydata_MAIDHA1$children_HH<- factor(mydata_MAIDHA1$children_HH, levels=c("None", "At least one"))

table(mydata_MAIDHA1$children_HH)

# cohabit

table(mydata_MAIDHA1$cohabit)

mydata_MAIDHA1$cohabit[mydata_MAIDHA1$cohabit==1] <-"No"
mydata_MAIDHA1$cohabit[mydata_MAIDHA1$cohabit==2] <-"Yes"

is.factor(mydata_MAIDHA1$cohabit)
mydata_MAIDHA1$cohabit<-as.factor(mydata_MAIDHA1$cohabit)
is.factor(mydata_MAIDHA1$cohabit)

table(mydata_MAIDHA1$cohabit)

mydata_MAIDHA1$cohabit<- factor(mydata_MAIDHA1$cohabit, levels=c("No", "Yes"))

table(mydata_MAIDHA1$cohabit)

# care_inside

table(mydata_MAIDHA1$care_inside)

mydata_MAIDHA1$care_inside[mydata_MAIDHA1$care_inside==1] <-"No"
mydata_MAIDHA1$care_inside[mydata_MAIDHA1$care_inside==2] <-"Yes"

table(mydata_MAIDHA1$care_inside)

is.factor(mydata_MAIDHA1$care_inside)
mydata_MAIDHA1$care_inside<-as.factor(mydata_MAIDHA1$care_inside)
is.factor(mydata_MAIDHA1$care_inside)

# care_outside 

table(mydata_MAIDHA1$care_outside)

mydata_MAIDHA1$care_outside[mydata_MAIDHA1$care_outside==0] <-"No"
mydata_MAIDHA1$care_outside[mydata_MAIDHA1$care_outside==1] <-"Yes"

is.factor(mydata_MAIDHA1$care_outside)
mydata_MAIDHA1$care_outside<-as.factor(mydata_MAIDHA1$care_outside)
is.factor(mydata_MAIDHA1$care_outside)

table(mydata_MAIDHA1$care_outside)

# new employment variable

table(mydata_MAIDHA1$new_employment)

mydata_MAIDHA1$new_employment[mydata_MAIDHA1$new_employment==1] <-"Employed FT"
mydata_MAIDHA1$new_employment[mydata_MAIDHA1$new_employment==2] <-"Employed PT"
mydata_MAIDHA1$new_employment[mydata_MAIDHA1$new_employment==3] <-"Other"

is.factor(mydata_MAIDHA1$new_employment)
mydata_MAIDHA1$new_employment<-as.factor(mydata_MAIDHA1$new_employment)
is.factor(mydata_MAIDHA1$new_employment)

mydata_MAIDHA1$new_employment<- factor(mydata_MAIDHA1$new_employment, levels=c("Employed FT", "Employed PT","Other"))

table(mydata_MAIDHA1$new_employment) 

# income_INDI

table(mydata_MAIDHA1$income_INDI)

mydata_MAIDHA1$income_INDI[mydata_MAIDHA1$income_INDI==1] <-"Low"
mydata_MAIDHA1$income_INDI[mydata_MAIDHA1$income_INDI==2] <-"High"

is.factor(mydata_MAIDHA1$income_INDI)
mydata_MAIDHA1$income_INDI<-as.factor(mydata_MAIDHA1$income_INDI)
is.factor(mydata_MAIDHA1$income_INDI)

table(mydata_MAIDHA1$income_INDI)

mydata_MAIDHA1$income_INDI<- factor(mydata_MAIDHA1$income_INDI, levels=c("Low", "High"))

table(mydata_MAIDHA1$income_INDI)

# setting reference category of outcome 

mydata_MAIDHA1$care_outside<-relevel(mydata_MAIDHA1$care_outside, ref="No")

# Logistic- Fit the two-level logistic regression with no covariates (Model2A) ####
model2A <- glmer(care_outside ~ (1|stratum), data=mydata_MAIDHA1, family=binomial)
summary(model2A)

tab_model(model2A)

# Predict the fitted linear predictor (on the probability scale)
mydata_MAIDHA1$m2Axbu <- predict(model2A, type="response")

# Predict the linear predictor for the fixed portion of the model only
mydata_MAIDHA1$m2Axb <- predict(model2A, type="response", re.form=NA)

# Fit the two-level linear regression with covariates ####
model2B <- glmer(care_outside ~ sex + children_HH + care_inside + cohabit+ new_employment+ income_INDI+age_con+
                   (1|stratum), data=mydata_MAIDHA1, family=binomial)
summary(model2B)

# Get the estimates as Odds ratios (and SEs on the odds scale)
tab_model(model2B, show.se=T)

# interpretation here (standard odds ratio interpretation, all with reference to reference category)

# predict the fitted linear predictor, and confidence intervals, on the logit scale (log odds)
m2Bm <- predictInterval(model2B, level=0.95, include.resid.var=FALSE)

# create a new id variable for this newly created dataframe (this is used to merge data into tut)
m2Bm <- mutate(m2Bm, id=row_number())

# on the logit scale, predict the linear predictor for the fixed portion of the model only
mydata_MAIDHA1$m2BmF <- predict(model2B, re.form=NA)

# predict the fitted linear predictor, and confidence intervals, on the probability scale (probabilty)
m2Bm_prob <- predictInterval(model2B, level=0.95, include.resid.var=FALSE, 
                             type="probability") 

# create a new id variable for this newly created dataframe (this is used to merge data into tut)
m2Bm_prob <- mutate(m2Bm_prob, id=row_number())

# predict the fitted linear predictor, on the probability scale, for the fixed portion of the model only
mydata_MAIDHA1$m2Bxb <- predict(model2B, re.form=NA, type="response")

#predict the stratum random effects and associated standard errors
# m2Bu will be used later to plot the caterpillar plot of the predicted stratum random/interaction effects
m2Bu <- REsim(model2B)

# first, merge predictions with original data

# create an id variable for merging in the tut dataframe
mydata_MAIDHA1$id <- seq.int(nrow(mydata_MAIDHA1))

# create a new dataframe, tut3, that merges mydata_MAIDHA1 and m2Bm_prob 
tut3 <- merge(mydata_MAIDHA1, m2Bm_prob, by="id")

# rename the m2BM_prob variable

tut3 <- tut3 %>%
  rename(
    m2Bmfit=fit,
    m2Bmupr= upr,
    m2Bmlwr=lwr
  )

# merge in m2Bm
tut3 <- merge(tut3, m2Bm, by="id")

# rename the m2BM variable

tut3 <- tut3 %>%
  rename(
    m2BmfitL=fit,
    m2BmuprL= upr,
    m2BmlwrL=lwr
  )
# Collapse the data down to a stratum-level dataset 

stratum_levelNEW <- aggregate(x=tut3[c("care_outside","m2Bmfit", "m2Bmupr", "m2Bmlwr", 
                                       "m2BmfitL", "m2BmuprL", "m2BmlwrL",
                                       "m2BmF")], 
                              by=tut3[c("sex", "children_HH", "care_inside","cohabit","new_employment","income_INDI",
                                        "stratum", "strataN")],
                              FUN=mean)

# convert the outcome from a proportion to a percentage
stratum_levelNEW$care_outside <- stratum_levelNEW$care_outside*100

View(stratum_levelNEW)

# Logistic Model- generating data for output data ####

# Create a table that includes all model estimates, including the Variance Partitioning Coefficients (VPC)
tab_model(model2A, model2B, p.style="stars")

# calculating VPC for model2A

summary(model2A)

# calculating VPC for model2B

summary(model2B)

# now calculating Proportional Change in Variance (PVCS)

# first extract variance matrices from the model objects
vc2a <-as.data.frame(VarCorr(model2A))
vc2b <-as.data.frame(VarCorr(model2B))

# calculate PCVs using components of these variance matrices (as percentages)
PCV2 <- ((vc2a[1,4] - vc2b[1,4]) / vc2a[1,4])*100
PCV2

# Logistic Models- visualising/calculating predicted stratum means ####

str(stratum_levelNEW)

# Rank the predicted stratum probabilities
stratum_levelNEW <- stratum_levelNEW %>%
  mutate(rank2=rank(m2Bmfit))

# convert probabilities to percentages
stratum_levelNEW$m2Bmfit <- stratum_levelNEW$m2Bmfit * 100
stratum_levelNEW$m2Bmupr <- stratum_levelNEW$m2Bmupr * 100
stratum_levelNEW$m2Bmlwr <- stratum_levelNEW$m2Bmlwr * 100

# Plot the caterpillar plot of the predicted stratum means 
p.1<-ggplot(stratum_levelNEW, aes(y=m2Bmfit, x=rank2)) +
  geom_point() +
  geom_pointrange(aes(ymin=m2Bmlwr, ymax=m2Bmupr)) +
  ylab("Predicted Percentage of Providing Informal Care") +
  xlab("Stratum Rank") + 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))

p.1


# Generate list of 6 highest and 6 lowest predicted stratum means
stratum_levelNEW <- stratum_levelNEW[order(stratum_levelNEW$rank2),]
head(stratum_levelNEW)
tail(stratum_levelNEW)

stratum_levelNEW

# to view entire list 
View(stratum_levelNEW)

# creating figure 2

stratumsim <- rbind(stratum_levelNEW, 
                    stratum_levelNEW[rep(1:nrow(stratum_levelNEW),999),])


stratumsim$m2Bmse <- (stratumsim$m2BmfitL - stratumsim$m2BmlwrL)/1.96

set.seed(354612)

stratumsim$m2Bpsim <- 100*invlogit(stratumsim$m2BmfitL + 
                                     rnorm(88000, mean=0, sd=stratumsim$m2Bmse))

stratumsim$m2BpAsim <- 100*invlogit(stratumsim$m2BmF)

stratumsim$m2BpBsim <- stratumsim$m2Bpsim - stratumsim$m2BpAsim

stratumsim <- stratumsim[order(stratumsim$stratum),]

stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m2BpBsim), std=sd(m2BpBsim)) %>%
  mutate(rank=rank(mean)) %>%
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std))

# plot the caterpillar plot of the predicted stratum percentage differences
p.2<-ggplot(stratumsim2, aes(x=rank, y=mean)) +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  xlab("Stratum Rank") +
  ylab("Difference in Predicted Percentage of Providing Informal Care due to Interactions") +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 90, by = 10), limits = c(0, 90))

p.2




