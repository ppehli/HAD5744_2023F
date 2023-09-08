########## Lecture4_Code.R
# Creator: Alex Hoagland, alcobe@bu.edu
# Created: 6/11/2022
# Last modified: 6/11/2022
#
# PURPOSE
#   1. OLS Regression (Multiple)
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

###### 1. Interaction Terms #####
res <- causaldata::restaurant_inspections

# Question: What is the effect of education on wages?
mydata <- causaldata::close_college

# First pass: simple regression
m1 <- lm(lwage ~ educ + exper, data=mydata)
msummary(list(m1),
         stars=c('*' = .1, '**' = .05, '***' = .01)) 

# How does the effect of education differ across race?
m2 <- lm(lwage ~ educ + exper + black + educ*black, data=mydata)
msummary(list(m1,m2),
         stars=c('*' = .1, '**' = .05, '***' = .01)) 

# What if we had left out the main effect?
m3 <- lm(lwage ~ educ + exper + educ:black, data=mydata)
msummary(list(m1,m2,m3),
         stars=c('*' = .1, '**' = .05, '***' = .01)) 
###################################


###### 2. Polynomial Effects ####
# Same question: what is effect of experience on wages? 
ggplot(data=mydata,aes(x=exper,y=lwage)) + geom_point() + 
  theme_minimal() + labs(x="Experience (Years)",y="Wages") + 
  geom_smooth(method='lm', formula= y~x)

# What if we add in a squared term? 
ggplot(data=mydata,aes(x=exper,y=lwage)) + geom_point() + 
  theme_minimal() + labs(x="Experience (Years)",y="Wages") + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red")

# What about a cubic term? 
ggplot(data=mydata,aes(x=exper,y=lwage)) + geom_point() + 
  theme_minimal() + labs(x="Experience (Years)",y="Wages") + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 3, raw=TRUE),colour="red")

# Higher order terms? 
n <- 10 # You pick a polynomial
ggplot(data=mydata,aes(x=exper,y=lwage)) + geom_point() + 
  theme_minimal() + labs(x="Experience (Years)",y="Wages") + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, n, raw=TRUE),colour="red")
################################


###### 3. Standard Errors ######
# 1. Heteroskedasticity
mydata <- mydata %>% mutate(wage=exp(lwage))
ggplot(mydata,aes(x=educ,y=wage)) + geom_point() + theme_minimal()

m1 <- lm(lwage ~ educ + exper + black + married + nearc4 + educ:black + exper:black, data=mydata)
msummary(list(m1),
         stars=c('*' = .1, '**' = .05, '***' = .01))

# Same model with robust standard errors
msummary(list(m1,m1),
         vcov=c("classical","robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))

# 2. Clustering
# Suppose that we have data on geography (US State)
mydata$state <- unlist(causaldata::abortion[1:3010,1])
m1 <- lm(lwage ~ educ + exper + black + married + nearc4 + educ:black + exper:black, data=mydata)

msummary(list(m1,m1,m1),
         vcov=c("classical","robust",~state),
         stars=c('*' = .1, '**' = .05, '***' = .01))

# 3. Bootstrapping
numiter <- 1000 # Number of times you want to sample
sizesample=nrow(mydata) # Size of the sample you want to take

# Create a place to store the coefficients
allbetas <- rep(NA,numiter)

# Run the loop
for (i in 1:numiter) { 
  sampledata <- mydata[sample(nrow(mydata), sizesample,replace=T), ]
  samplemodel <- lm(lwage ~ educ + exper + black + married + nearc4 + educ:black + exper:black, data=sampledata)
  allbetas[i] <- samplemodel$coefficients[2] # Effect of education on lwage
}

# Represent graphically
allbetas <- tibble(allbetas)
ggplot(allbetas,aes(x=allbetas)) + geom_histogram(fill='gray',color="black") + theme_minimal() + 
  labs(x="Estimated Betas",y="Count") + geom_vline(xintercept=0.077,color="red",size=2)

sd(allbetas$allbetas) # This gives us a bootstrapped SE remarkably similar to the regression approach
###############################

