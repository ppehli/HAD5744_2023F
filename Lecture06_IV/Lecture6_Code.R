########## Lecture6_Code.R
# Creator: Alex Hoagland, alcobe@bu.edu
# Created: 6/30/2022
# Last modified: 
#
# PURPOSE
#   IV Regression
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

set.seed(03262020)
##########


##### 0. Simulating Data #####
mydata <- data.frame(
  id = 1:10000,
  age = sample(seq(0,100,1),size=10000,replace=T),
  blood_type = sample(c("O_pos", "A_pos", "B_pos", "O_neg", "A_neg", "AB_pos", "B_neg", "AB_neg"), size=10000, replace=T, 
                      prob=c(0.38, 0.34, 0.09, 0.07, 0.06, 0.03, 0.02, 0.01)), # https://www.statista.com/statistics/1112664/blood-type-distribution-us/
  wait_time = rexp(n=10000) * 100 # Waiting times in days
)

# Correlation between blood type and waiting times
mydata <- mydata %>% mutate(wait_time = ifelse(blood_type == "A_pos",wait_time * 0.12, 
                                               ifelse(blood_type == "A_neg", wait_time * 0.12,
                                                      ifelse(blood_type == "B_pos", wait_time * 0.46,
                                                             ifelse(blood_type == "B_neg", wait_time * 0.46,
                                                                    ifelse(blood_type == "AB_pos", wait_time * 0.05,
                                                                           ifelse(blood_type == "AB_neg", wait_time * 0.05,
                                                                                  ifelse(blood_type == "O_pos", wait_time * 0.54,
                                                                                         wait_time * 0.54))))))))

# Add in outcome variable
mydata <- mydata %>% mutate(mortality = ifelse(wait_time > 180, sample(c(0,1), size=10000, replace = T, prob = c(0.75, 0.25)), 
                                               ifelse(wait_time > 90, sample(c(0,1), size=10000, replace = T, prob = c(0.8, 0.2)),
                                                      ifelse(wait_time > 60, sample(c(0,1), size=10000, replace = T, prob = c(0.9, 0.1)),
                                                             sample(c(0,1), size=10000, replace = T, prob = c(0.97, 0.03)))))) # Probability of mortality depends on wait times 

# Bake in some endogeneity -- something unseen (age) that causes both X and Y 
mydata <- mydata %>% mutate(wait_time = wait_time * abs(age-30)/sd(age), 
                            mortality = round(mortality * abs(age-30)))
mydata[which(mydata$mortality>1),]$mortality <- 1 # Only let mortality be factor variable
##############################


##### 1. IV "by hand" #####
# first, simple regression -- what is "simplistic" effect of wait times on mortality? 
mydata <- mydata %>% mutate(wt_months = wait_time / 30 ) # Measure wait times in months
m1 <- lm(mortality ~ wt_months,data=mydata)
msummary(list(m1),
         vcov=c("robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
  # Strong positive effect -- longer wait times equals higher mortality. 

# First stage regression of IV: Wait times on blood type
mydata <- mydata %>% mutate(rare = ifelse(blood_type %in% c("O_pos","O_neg","B_pos","B_neg"), 1, 0))
m2 <- lm(wt_months ~ rare,data=mydata)
msummary(list(m2),
         vcov=c("robust"),
         tab_header=c("First Stage"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
# Strong positive effect -- rare blood types wait a month and a half longer than others

# Build second stage by predicting X with Z: 
mydata$pred_x <- predict(m2, mydata) # Note that predicted X only takes on two values -- why?

# Regress outcome on pred_X
m3 <- lm(mortality ~ pred_x,data=mydata)
msummary(list(m1,m3),
         vcov=c("robust","robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
# Once instrumented, the effect is (statistically) *higher* 

# Testing relevance condition
msummary(list(m2),
         vcov=c("robust"),
         tab_header=c("First Stage"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
###############################


##### 2. IV in practice #####
# There are many ways to run 2SLS: the most common is ivreg from the AER package. 
# Another good option is feols from fixest 
library(AER) # this package has lots of applied metrics packages

m4 <- ivreg(mortality ~ wt_months | blood_type, data=mydata,x=T) # the function of use is ivreg
msummary(list("Simplistic"=m1,"IV By Hand"=m3,"IV By AER"=m4),
         vcov=c("robust","robust","robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))

# Corrections for weak instruments
# Easiest correction: use Anderson-Rubin confidence intervals 
# Citation: Anderson, Theodore W, and Herman Rubin. 1949. “Estimation of the Parameters of a Single Equation in a Complete System of Stochastic Equations.” The Annals of Mathematical Statistics 20 (1): 46–63.

# Requires downloading an archived package
url <- "https://cran.r-project.org/src/contrib/Archive/ivpack/ivpack_1.2.tar.gz"
pkgFile <- "ivpack_1.2.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
library(ivpack) 

ivpack::anderson.rubin.ci(m4, conflevel = 0.95)
##############################


###### 3. IV by GMM ######
library(gmm)

m5 <- gmm(mortality ~ wt_months, # Main regression
         ~ blood_type, data = mydata)

# We can apply the address clustering most easily in msummary
msummary(list("GMM"=m5),
         stars=c('*' = .1, '**' = .05, '***' = .01))
#########################