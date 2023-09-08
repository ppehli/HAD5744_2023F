########## Lecture9_Code.R
# Creator: Alex Hoagland, alcobe@bu.edu
# Created: 7/18/2022
# Last modified: 
#
# PURPOSE
#   Difference-in-differences
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


##### 1. Testing Parallel Trends #####
# Dataset: COVID Vaccination rate by US state 
library(zoo)
mydata <- read.csv("C://Users//alexh//Dropbox//Teaching//HAD5744//2022_Fall//HAD5744_2022F_Github//Lecture9_DifferenceinDifferences//us_state_vaccinations.csv")
mydata$date <- as.yearmon(mydata$date)

# Trim out some "states" that we don't need
`%!in%` <- Negate(`%in%`)
mydata <- mydata %>% filter(location  %!in% c("American Samoa", "Bureau of Prisons", "Dept of Defense",
                                             "Federated States of Micronesia", "Guam", "Indian Health Svc",
                                             "Long Term Care", "Marshall Islands", "Northern Mariana Islands",
                                             "Puerto Rico", "Republic of Palau", "United States", "Veterans Health",
                                             "Virgin Islands"))

# Group to month level
mydata <- mydata %>% group_by(date, location) %>% 
  summarize(total_vaccinations_per_hundred = sum(total_vaccinations_per_hundred,na.rm=T),
            people_vaccinated_per_hundred = sum(people_vaccinated_per_hundred,na.rm=T),
            monthly_vaccinations_per_million = sum(daily_vaccinations_per_million,na.rm=T))
#Show rates of donation over time for each state
ggplot(mydata,aes(x=date,y=monthly_vaccinations_per_million,group=location)) + geom_line() + 
  theme_classic() + labs(x="Time",y="Monthly Vaccinations per Million")

# Look at two similar states: Ohio (vaccine lottery started June 2021) and Michigan
ggplot(mydata[which(mydata$location=="Ohio"),],aes(x=date,y=monthly_vaccinations_per_million,group=location)) + geom_line() + 
  theme_classic() + labs(x="Time",y="Monthly Vaccinations per Million")
ggplot(mydata[which(mydata$location=="Michigan"),],aes(x=date,y=monthly_vaccinations_per_million,group=location)) + geom_line() + 
  theme_classic() + labs(x="Time",y="Monthly Vaccinations per Million")

# Regression data -- just ohio and michigan 
regdata <- mydata %>% filter(location %in% c("Michigan","Ohio"))

# Definition of key variables
regdata <- regdata %>% mutate(timegroup = ifelse(location == "Ohio",date,0))

# Statistical tests: are the two groups' trends different?
pretrend_test <- lm(monthly_vaccinations_per_million ~ date + timegroup, data=regdata)
msummary(list(pretrend_test),
         vcov=c(rep("robust",1)),
         stars=c('*' = .1, '**' = .05, '***' = .01)) # Interpret each coefficient here
###############################


##### 2. Two-way DiD ####
# Definition of key variables
regdata <- regdata %>% mutate(state = ifelse(location == "Ohio",1,0),
                              post = ifelse(date >= "Jun 2021",1,0),
                              inter = state * post)

# Main regression
did_simple <- lm(monthly_vaccinations_per_million ~ state + post + inter, data=regdata)
msummary(list("Simple DID"=did_simple),
         vcov=c(rep("robust",1)),
         stars=c('*' = .1, '**' = .05, '***' = .01)) # Interpret each coefficient here

# Show parallel trends when you transform the data
regdata <- regdata %>% mutate(logy = log(monthly_vaccinations_per_million))

# Graph of both over time
ggplot(data=regdata,aes(x=date,y=monthly_vaccinations_per_million,group=state,color=factor(state))) + 
  geom_line() + theme_classic() + labs(x="Date",y="Montly Vaccinations",color="State = Ohio") + 
  geom_vline(xintercept = 2021.417,size=1.5,color='red',linetype='dashed')
ggplot(data=regdata,aes(x=date,y=logy,group=state,color=factor(state))) + 
  geom_line() + theme_classic() + labs(x="Date",y="Montly Vaccinations",color="State = Ohio") + 
  geom_vline(xintercept = 2021.417,size=1.5,color='red',linetype='dashed')

# Test of trend differences in logs
pretrend_test2 <- lm(logy ~ date + timegroup, data=regdata)
msummary(list("Levels"=pretrend_test,"Logs"=pretrend_test2),
         vcov=c(rep("robust",2)),
         stars=c('*' = .1, '**' = .05, '***' = .01)) # May consider using log(y) anyway -- why?
#######################################


##### 3. Generalized DiD ####
# Let's try expanding to all states, with state and time fixed effects
library(fixest)
mydata <- mydata %>% mutate(state = ifelse(location == "Ohio",1,0),
                              post = ifelse(date >= "Jun 2021",1,0),
                              inter = state * post,
                              logy = log(monthly_vaccinations_per_million+1))

# feols clusters by the first
# fixed effect by default, no adjustment necessary
did_full <- feols(logy ~ inter | location + date,
              data = mydata)
msummary(list("Simple"=did_simple,"Full"=summary(did_full)),
         stars = c('*' = .1, '**' = .05, '***' = .01)) # Why is this different? What makes it more precise? What makes it less believable?

# Placebo Regression: Changing Treatment Dates
placdata <- mydata %>% filter(date < "Jun 2021")
placdata <- placdata %>% mutate(newtreat = ifelse(location == "Ohio" & date >= "Mar 2021",1,0))
did_plac <- feols(logy ~ newtreat | location + date,
                  data = placdata)
msummary(list("Simple"=did_simple,"Full"=summary(did_full),"Placebo"=summary(did_plac)),
         stars = c('*' = .1, '**' = .05, '***' = .01)) # What does this tell us? 

# Can also do one where you change treatment IDs rather than dates
######################################


##### 4. TWFE ####
## Single treatment event
# First, construct relative time variable
regdata_dte <- mydata %>% mutate(reltime = round((as.numeric(date) - 2021.417)*12)) 
dte <- feols(logy ~ i(reltime, state, ref = -1) | 
                date + location, data = regdata_dte)

# And use coefplot() for a graph of effects
iplot(dte,ref.line=-0.2, 
         ref.line.par=list(col="red",lty=1,lwd=2)) # How do we interpret this? Pre-trends? What does the coefficient on -1 tell us? What do positive coefficeints tell us?

# What if we limit attention to just states surrounding Ohio? 
regdata_dte <- regdata_dte %>% filter(location %in% c("Ohio", "Michigan", "Indiana", "Pennsylvania", "West Virginia", "Kentucky"))
dte2 <- feols(logy ~ i(reltime, state, ref = -1) | 
               date + location, data = regdata_dte)

# And use coefplot() for a graph of effects
iplot(dte2,ref.line=-0.2, 
      ref.line.par=list(col="red",lty=1,lwd=2)) # How do we interpret this? Pre-trends? What does the coefficient on -1 tell us? What do positive coefficeints tell us?

## TWFE with staggered timing
# Now, the relative time variable will be different for different states -- not just centered around 2021.417
# Other states: May 2021: NY, MD; July 2021: MA, MI (there are others)
regdata_dte <- mydata %>% mutate(treatdate = ifelse(location %in% c("Ohio", "New York State", "Maryland"), 2021.417,
                                                     ifelse(location %in% c("Massachusetts", "Michigan"), 2021.583,NA)), # Note the treatment date doesn't matter for non-treated states
                                 reltime = round((as.numeric(date) - treatdate)*12)) 
regdata_dte <- regdata_dte %>% mutate(state = ifelse(location %in% c("Ohio", "New York State", "Maryland", "Massachusetts", "Michigan"), 1, 0))
  # Make sure to reassign "treated" states

dte <- feols(logy ~ i(reltime, state, ref = -1) | 
               date + location, data = regdata_dte)

# And use coefplot() for a graph of effects
iplot(dte,ref.line=-0.2, 
      ref.line.par=list(col="red",lty=1,lwd=2)) # How do we interpret this? 
######################################