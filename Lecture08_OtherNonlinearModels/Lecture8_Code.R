########## Lecture7_Code.R
# Creator: Alex Hoagland, alcobe@bu.edu
# Created: 7/15/2022
# Last modified: 
#
# PURPOSE
#   Limited Dependent Variables
#
# NOTES: 
#   - uses the Tidyverse package and Dplyr
################################################################################


##### Packages #####
# install.packages('tidyverse') # if needed, install the package
library(tidyverse) # call the relevant library
library(faux) # Useful package for simulating data
library(modelsummary) 
library(causaldata) 
library(here)
library(foreign)

set.seed(03262020)
##########


##### 1. Linear Probability Models #####
# Dataset: Effect of health insurance on expenditures
mydata <- readxl::read_xlsx("C:\\Users\\alexh\\Dropbox\\Teaching\\HAD5744\\2022_Fall\\HAD5744_2022F_Github\\Lecture5_Matching\\HealthExpenses.xlsx")

# Main (binary) outcome: who has coverage?
m1 <- lm(coverage ~ age + sex + bmi + children + smoker,data=mydata)
msummary(list(m1),
         vcov=c("robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
# How do we interpret these coefficients?

# Look at predictions
mydata$pred_coverage <- predict(m1, mydata)
hist(mydata$pred_coverage) # what problems do you see here? 
summary(mydata) # First problem: some predictions are outside of the unit interval!
mydata %>% filter(pred_coverage > 1.14) # What does it mean to have a 114% probability of having coverage?

hist(mydata$pred_coverage) # Second problem: how do you go from a predicted 57% probability of Y=1 to predicting Y? 
mydata <- mydata %>% mutate(yhat = ifelse(pred_coverage >= 0.5, 1, 0)) # What happens if we assume a cutoff (Yhat >= 50% means Yhat = 1)? How good is our match then?
table(mydata$coverage, mydata$yhat) # We incorrectly match 34% of the data!
###############################


##### 2. Logit and probit models #####
# Logit
m2 <- glm(coverage ~ age + sex + bmi + children + smoker,data=mydata,
          family='binomial'(link='logit')) # since we have binary data

# Probit
m3 <- glm(coverage ~ age + sex + bmi + children + smoker,data=mydata,
          family='binomial'(link='probit')) # since we have binary data

msummary(list("LPM"=m1,"Logit"=m2,"Probit"=m3),
         vcov=c(rep("robust",3)),
         stars=c('*' = .1, '**' = .05, '***' = .01))
# What do these coefficients mean? Why are the logit and probit so different? 

##  Interpreting coefficients: marginal effects (done for logit only -- can you do it for probit?)
# 1. Each individual marginal effect
library(devtools)
install_github("vincentarelbundock/marginaleffects")
library(marginaleffects)
allmarginaleffects <- marginaleffects(m2) # Gives you individual ME for all variables/observations
View(allmarginaleffects) # sort by p-value to see high-impact factors

# 2. and 4. MER and MEM: use the datagrid() function
summary(marginaleffects(m2, datagrid(age  =50, sex = "male", bmi = 50, children = 0, smoker = 1, grid.type = 'counterfactual')))

# 3. AME
summary(marginaleffects(m2)) # This gives you AME
plot(marginaleffects(m2)) # Convenience plot 

# 4. MEM: 
summary(marginaleffects(m2, datagrid()))


## Testing: Wald test
# Say we want to test joint hypothesis of age and sex on coverage decisions
# Full model is m2 -- already estimated
restricted <- glm(coverage ~ bmi + children + smoker,data=mydata,
          family='binomial'(link='logit')) # Restricted model is where age and sex are both 0
lmtest::waldtest(m2,restricted) # what does this tell us?
#######################################


##### 3. Poisson Regression ####
# Suppose our outcome here is number of health visits
## Some simulation (ignore this) 
mydata$numvisits <- rpois(1340,2) # Base numvisits
# Now bake in some correlation
mydata <- mydata %>% 
  mutate(numvisits = ifelse(age >= 65,round(numvisits*1.5),numvisits)) %>%
  mutate(numvisits = ifelse(age < 35,round(numvisits*0.5),numvisits)) %>%
  mutate(numvisits = ifelse(sex == "male",round(numvisits*0.8),numvisits)) %>%
  mutate(numvisits = numvisits * bmi/31) %>%
  mutate(numvisits = ifelse(numvisits > 10, 10, numvisits)) %>% 
  mutate(numvisits = ifelse(smoker == 1, numvisits * 2, numvisits)) %>% 
  mutate(numvisits = numvisits * expenses/9000) %>% 
  mutate(numvisits = ifelse(numvisits > 10, 10, numvisits)) %>% 
  mutate(numvisits = ifelse(coverage == 1,round(numvisits*1.2),numvisits))
mydata$numvisits <- as.integer(mydata$numvisits)
hist(mydata$numvisits)


# Now, estimate a Poisson regression on all of our variables. what is impact of coverage?
m_poisson <- glm(numvisits ~ coverage + age + sex + bmi + children + smoker,data=mydata,
                 family = "poisson"(link = "log")) # Why is our link function the log function?
m_naive <- lm(numvisits ~ coverage + age + sex + bmi + children + smoker,data=mydata) # Naive OLS
msummary(list("Naive"=m_naive,"Poisson"=m_poisson),
         vcov=c(rep("robust",2)),
         stars=c('*' = .1, '**' = .05, '***' = .01))

exp(m_poisson$coefficients)-1 # What is the marginal effect for coverage? What is the rate ratio?

# Test for overdispersion in "catastrophic"
var(mydata$numvisits)/mean(mydata$numvisits) # Lots of overdisperson! Could bootstrap this to get a CI. 
######################################


##### 4. Hurdle Model
# How does our Poisson model do in terms of predicted zeros? 
mydata$predicted_visits <- predict(m_poisson, type = "response")
ggplot(mydata)+
  geom_histogram(aes(x=numvisits),position='dodge',fill='blue',alpha=0.3)+
  geom_histogram(aes(x=predicted_visits),position='dodge',fill='red',alpha=0.5)+
  theme_classic()+
  labs(x="# of Visits",y="Count")

# sum the probabilities of a 0 count for each mean
exp <- sum(dpois(x = 0, lambda = mydata$predicted_visits))
round(exp) # predicted number of 0's
sum(mydata$numvisits == 0) # Observed number of 0's

# Estimating the hurdle model -- additional variables? 
library(pscl)
mod.hurdle <- hurdle(numvisits ~ age + sex + bmi + children + smoker + coverage, data = mydata)
  # First, suppose we have same covariates in both stage of the model 

# Model summary doesn't work as well with this package
summary(mod.hurdle)$coefficients

# How does it do for prediction? 
sum(predict(mod.hurdle, type = "prob")[,1]) # Note: by design, hurdle model gives you exact number of 0s in data
mydata$new_pred <- floor(predict(mod.hurdle, type="response")) # Not sure that this prediction method is correct.
ggplot(mydata)+
  geom_histogram(aes(x=numvisits),position='dodge',fill='blue',alpha=0.3)+
  geom_histogram(aes(x=new_pred),position='dodge',fill='red',alpha=0.5)+
  theme_classic()+
  labs(x="# of Visits",y="Count")
##########################################


##### 5. Multinomial Logit Regression ####
# Assume now that everyone in our data set has an MD visit, and either chooses (0) no treatment, 
#                                                                              (1) medication, 
#                                                                           or (2) surgery.

## Some simulation (ignore this) 
mydata$choice <- sample(c(0,1,2),size=1340,replace=T,prob=c(0.5,0.25,0.25)) # Base choices
# Now bake in some correlation
mydata <- mydata %>% 
  mutate(choice = ifelse(age >= 65,round(choice*1.5),choice)) %>%
  mutate(choice = ifelse(choice > 2, 2, choice)) %>%
  mutate(choice = ifelse(age < 35,round(choice*0.5),choice)) %>%
  mutate(choice = ifelse(choice < 0, 0, choice)) %>%
  mutate(choice = ifelse(sex == "male",round(choice*1.5),choice)) %>%
  mutate(choice = ifelse(choice > 2, 2, choice)) %>%
  mutate(choice = ifelse(bmi >= 32 & choice == 2,sample(c(0,1),replace=T,prob=c(0.3,0.7)), choice)) %>%
  mutate(choice = ifelse(smoker == 1 & choice == 2,sample(c(0,1),replace=T,prob=c(0.5,0.5)), choice)) %>%
  mutate(choice = ifelse(smoker == 1 & choice == 1,sample(c(0,1),replace=T,prob=c(0.7,0.3)), choice)) %>%
  mutate(choice = ifelse(expenses >= 17000,round(choice*1.5),choice)) %>%
  mutate(choice = ifelse(choice > 2, 2, choice)) %>%
  mutate(choice = ifelse(coverage == 1,round(choice*1.5),choice)) %>%
  mutate(choice = ifelse(choice > 2, 2, choice))
hist(mydata$choice)

# Estimation
# First, we select our comparison outcome (in this case, no treatment)
library(nnet)
mydata$choice <- factor(mydata$choice,levels=c(0,1,2),labels=c("Monitoring","Medication","Surgery")) # Let's talk about factors
mydata$region <- factor(mydata$region,levels=c("northeast","northwest","southeast","southwest"),labels=c("northeast","northwest","southeast","southwest")) # Let's us include this straight and calculate dummies automatically
mydata$choice_relative <- relevel(mydata$choice, ref = "Monitoring")
mnl <- multinom(choice_relative ~ age + sex + bmi + children + smoker + expenses + coverage + region,
                data = mydata) # Discuss iterations and output

# Note: msummary doesn't work well for this kind of model
summary(mnl)

# Calculate our own significance
z <- summary(mnl)$coefficients/summary(mnl)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Presenting the model: some options
View(tidy(mnl, conf.int = TRUE)) # Can save this as an object to construct your own table

# an HTML version
library(knitr)
library(kableExtra)
tidy(mnl, conf.int = TRUE) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)

# This gives you a table of relative risk ratios
library(gtsummary)
tbl_regression(mnl, exp = TRUE)

# Interperting the model
exp(coef(mnl)) # this gives us relative risk ratios -- see slides about odds ratios

# Calculating marginal effects: 
library(marginaleffects)
myme <- marginaleffects(mnl, type = "probs")
# Warning: The standard errors estimated by `marginaleffects` do not match those
# produced by Stata for `nnet::multinom` models. Please be very careful when
# interpreting the results.
summary(myme)

# Checking accuracy of the model
mydata$predicted_choice <- predict(mnl,newdata=mydata,"class")
ctable <- table(mydata$choice_relative,mydata$predicted_choice) # Building classification table
ctable
round((sum(diag(ctable))/sum(ctable))*100,2) # Calculating accuracy - sum of diagonal elements divided by total obs
  # 71.12% accuracy doesn't sound bad. 

ggplot(data=mydata,aes()) + 
  geom_histogram(aes(x=as.numeric(choice)-.1),fill='blue',alpha=0.3,position='dodge') + 
  geom_histogram(aes(x=as.numeric(predicted_choice)+.1),fill='red',alpha=0.5,position='dodge') + 
  theme_minimal() + 
  labs(x="Choice",y="Count") # But don't forget to visualize it! 
######################################