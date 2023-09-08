########## Lecture3_Code.R
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
library(modelsummary) # For making regression tables
###################################


##### Multivariate Regression #####
# Create a data set of 1000 regions, with some rates of hospitalizations, A1Cs, and education (in years)
# Covariance matrix used to simulate the data
covmat <- matrix(c(1,.5,-.2,.5,1,.5,-.2,.5,1),nrow=3,ncol=3)
mydata <- rnorm_multi(1000,3,0,1,covmat,varnames=c("hospitalizations","a1cs","education"))
view(mydata)

# First regression: just hospitalization on A1Cs
lm_simple <- lm(hospitalizations ~ a1cs, data=mydata)
summary(lm_simple)

# Now, control for education 
lm_complex <- lm(hospitalizations ~ a1cs + education, data=mydata)
summary(lm_complex)
#########################################


##### 1. Dummy Variable Trap ######
mydata <- mydata %>% mutate(female = ifelse(runif(nrow(mydata))>0.6, 1, 0),
                            male = 1-female)

# Our dummy variable: what is the gender composition of our sample?
summary(mydata$female)

# Linear model with dummy variable included
lm_dummy <- lm(hospitalizations ~ a1cs + education + female, data=mydata)
summary(lm_dummy) # What does it mean that this is significant? Should it be? 
  # Return to this at the end of the lecture when we talk about validity traps

# Let's make a regression table for ease of interpretation
msummary(list("Simple"=lm_simple,"One Control"=lm_complex,"Full"=lm_dummy),
         stars=c('*' = .1, '**' = .05, '***' = .01))

# What happens if we include both male and female in a regression? 
lm_dummytrap <- lm(hospitalizations ~ a1cs + education + female + male, data=mydata)
summary(lm_dummytrap)
  # Code can usually detect the dummy variable trap, but don't count on it! 

# Multiple dummy variables: region 
mydata <- mydata %>% mutate(region = sample(seq(1:4),size=nrow(mydata),replace=TRUE)) 
  # Generate a random region variable for restaurants 

# Suppose that 1=West, 2=Midwest, 3=South, 4=East
# Need to generate three dummy variables for the regression 
mydata <- mydata %>% mutate(region_west = (region == 1), 
                      region_midwest = (region == 2), 
                      region_south = (region == 3))
lm_region <- lm(hospitalizations ~ a1cs + education + female + region_west + region_midwest + region_south, data=mydata)
msummary(list("No Regions"=lm_dummy, "With Region Controls"=lm_region),
         stars=c('*' = .1, '**' = .05, '***' = .01)) # Why aren't our other coefficients changed? Thoughts? 

# # Joint test of significance
# library('lmtest')
# lmtest::waldtest(lm_dummy, lm_region) # Run a version of the regression without the variables you want to test, 
# # and one with full set of coefficients. Then use waldtest to test difference
##################################


##### Inference #####
# How to obtain SEs
myout <- summary(lm_complex)
myout 
myout$coefficients[,2]

# Start with a model where y = 3 + .2x + epsilon, e ~ N(0,1)
set.seed(0326)
mydata <- tibble(x=rnorm(200),
                 epsilon=rnorm(200),
                 y=3+.2*x+epsilon)
m1 <- lm(y ~ x,data=mydata)
out <- summary(m1) # I get a first estimate of beta = 0.12, and a SE = 0.064
myse <- out$coefficients[2,2] # store the SE

# State the null hypothesis: beta_1 = 0. 
# Under this hypothesis, se(beta) = 0.064
myalf <- .05 # State the rejection probability

# Plot my estimate against the sampling distribution of the null hypothesis 
graphdata <- tibble(x=rnorm(1e6,mean=0,sd=myse), # normal distribution
                    xrej = ifelse(x >= qnorm(1-(myalf)/2, mean=0, sd=myse),x,NA), # x's that fall in the rejection region based on alpha
                    xrej2 = ifelse(-x >= qnorm(1-(myalf)/2, mean=0, sd=myse),x,NA), # x's that fall in the rejection region based on alpha
                    y=dnorm(x,mean=0,sd=myse)
)

 ggplot(data=graphdata) + 
  geom_density(aes(x)) + 
  geom_area(data=subset(graphdata, !is.na(xrej)),aes(x=x,y=y),fill="gray") + 
   geom_area(data=subset(graphdata, !is.na(xrej2)),aes(x=x,y=y),fill="gray") + 
  geom_vline(xintercept=0.12,linetype='dashed',color='red') + 
  theme_minimal() + 
  labs(x="Estimated Beta",y="Density")
 
# What is the conclusion of this test?
 
# What happens if I perform a one-sided test? 
 graphdata <- tibble(x=rnorm(1e6,mean=0,sd=myse), # normal distribution
                     xrej = ifelse(x >= qnorm(1-(myalf), mean=0, sd=myse),x,NA), # x's that fall in the rejection region based on alpha
                     y=dnorm(x,mean=0,sd=myse)
 )
 
 ggplot(data=graphdata) + 
   geom_density(aes(x)) + 
   geom_area(data=subset(graphdata, !is.na(xrej)),aes(x=x,y=y),fill="gray") + 
   geom_vline(xintercept=0.12,linetype='dashed',color='red') + 
   theme_minimal() + 
   labs(x="Estimated Beta",y="Density")
 
# What happens if I add more data and reduce the SE?
 mydata <- tibble(x=rnorm(20000),
                  epsilon=rnorm(20000),
                  y=3+.2*x+epsilon)
 m1 <- lm(y ~ x,data=mydata)
 out <- summary(m1) # I get a first estimate of beta = 0.12, and a SE = 0.064
 out
 myse <- out$coefficients[2,2] # store the SE
 
 graphdata <- tibble(x=rnorm(1e6,mean=0,sd=myse), # normal distribution
                     xrej = ifelse(x >= qnorm(1-(myalf)/2, mean=0, sd=myse),x,NA), # x's that fall in the rejection region based on alpha
                     xrej2 = ifelse(-x >= qnorm(1-(myalf)/2, mean=0, sd=myse),x,NA), # x's that fall in the rejection region based on alpha
                     y=dnorm(x,mean=0,sd=myse)
 )
 
 ggplot(data=graphdata) + 
   geom_density(aes(x)) + 
   geom_area(data=subset(graphdata, !is.na(xrej)),aes(x=x,y=y),fill="gray") + 
   geom_area(data=subset(graphdata, !is.na(xrej2)),aes(x=x,y=y),fill="gray") + 
   geom_vline(xintercept=0.12,linetype='dashed',color='red') + 
   theme_minimal() + 
   labs(x="Estimated Beta",y="Density")
 
# Calculate p-value of our estiamted beta
1-pnorm(out$coefficients[2,1],mean=0,sd=out$coefficients[2,2])

# Calculate the t-statistic of our estimated beta
out$coefficients[2,1]/out$coefficients[2,2] # Here, critical value is 1.96
########


##### Making a Regression Table ####
install.packages('modelsummary') # Helpful package for regressions -- there are others!
install.packages('causaldata') # Package to get the data from HK
install.packages('here') # Useful package for standardizing directories

library(modelsummary) 
library(causaldata)
library(here)

# Set the directory of interest
setwd("C:/Users/alexh/Dropbox/Teaching/HAD5744/2022_Fall")
here::i_am("Lecture3/Lecture3_Code.R")

res <- causaldata::restaurant_inspections

res <- res %>%
  # Create NumberofLocations
  group_by(business_name) %>%
  mutate(NumberofLocations = n())
Summary(res$NumberofLocations)

# Perform the first, one-predictor regression
# use the lm() function, with ~ telling us what 
# the dependent variable varies over
m1 <- lm(inspection_score ~ NumberofLocations, data = res)

# Now add year as a control
# Just use + to add more terms to the regression
m2 <- lm(inspection_score ~ NumberofLocations + Year, data = res)

# Give msummary a list() of the models we want in our table
# and save to a file using the here() library
# Note that you select the file type (html, pdf, etc.)
# (see help(msummary) for other options)
msummary(list(m1, m2),
         stars=TRUE,
         output= here("Lecture4", 'regression_table.html')) 
# Default significance stars are +/*/**/*** .1/.05/.01/.001. Social science
# standard */**/*** .1/.05/.01 can be restored with
msummary(list(m1, m2),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         output= here("Lecture4", 'regression_table.html'))
##########