
## Project:  SOC 302 Final Multivariate Project
# Located:   Class Folder on ELSA
# File Name: needham-gun_law.R
# Date:      3/31/2025
# Who:       Jenna Needham


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
setwd("/courses/SOC302/needhaj1")
install.packages("dpylr")
install.packages("psych")

library(dplyr)
library(psych)


### Load data 
GSS <- read.csv("GSS2022.csv")


####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()

table(GSS$gunlaw)

summary(GSS$gunlaw)



############                     DEPENDENT VARIABLE                     ############
############                   Support or Oppose Gun Permits            ############

# STEP 1: Examine variable and coding schema 

table(GSS$gunlaw)

# STEP 2: Create dummy variables for each category of confidence in gun law
GSS <- mutate(GSS, favor_gun = ifelse(gunlaw == 1, 1, 0))
GSS <- mutate(GSS, oppose_gun = ifelse(gunlaw == 2, 1, 0))

# STEP 3: Confirm creation at gunlaw and each dummy variable 
table(GSS$favor_gun)
table(GSS$oppose_gun)
table(GSS$gunlaw, GSS$favor_gun)
table(GSS$gunlaw, GSS$oppose_gun)


############                  INDEPENDENT VARIABLE                    ############
############               Does Respondent Own a Gun                  ############

# STEP 1: Examine variable and coding schema 

table(GSS$owngun)

# STEP 2: Dummy variables for each answer
GSS <- mutate(GSS, has_gun = ifelse(owngun == 1, 1, 0))
GSS <- mutate(GSS, no_gun = ifelse(owngun == 2, 1, 0))


GSS <- mutate(GSS, refused = ifelse(owngun == 3, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$owngun, GSS$has_gun)
table(GSS$owngun, GSS$no_gun)
table(GSS$owngun, GSS$refused)



############                  INDEPENDENT VARIABLE                    ############
############               Respondent's Political Identity            ############

# STEP 1: Examine variable and coding schema 

table(GSS$partyid)

# STEP 2: Dummy variables for each answer
GSS <- mutate(GSS, strong_democrat = ifelse(partyid == 0, 1, 0))
GSS <- mutate(GSS, low_democrat = ifelse(partyid == 1, 1, 0))
GSS <- mutate(GSS, liberal_independent = ifelse(partyid == 2, 1, 0))
GSS <- mutate(GSS, mid_independent = ifelse(partyid == 3, 1, 0))
GSS <- mutate(GSS, conservative_independent = ifelse(partyid == 4, 1, 0))
GSS <- mutate(GSS, low_republican = ifelse(partyid == 5, 1, 0))
GSS <- mutate(GSS, strong_republican = ifelse(partyid == 6, 1, 0))
GSS <- mutate (GSS, other_party = ifelse(partyid == 7, 1, 0))     


table(GSS$strong_democrat)
table(GSS$low_democrat)
table(GSS$liberal_independent)
table(GSS$mid_independent)
table(GSS$conservative_independent)
table(GSS$low_republican)
table(GSS$strong_republican)
table(GSS$other_party)


# STEP 3: Confirm creation (if necessary)
table(GSS$partyid, GSS$strong_democrat)
table(GSS$partyid, GSS$low_democrat)
table(GSS$partyid, GSS$liberal_independent)
table(GSS$partyid, GSS$mid_independent)
table(GSS$partyid, GSS$conservative_independent)
table(GSS$partyid, GSS$low_republican)
table(GSS$partyid, GSS$strong_republican)      
table(GSS$partyid, GSS$other_party)      


############                  INDEPENDENT VARIABLE                    ############
############                        Region                             ############

# STEP 1: Examine variable and coding schema 

table(GSS$reg16)

# STEP 2: Dummy variables for each answer


GSS <- mutate(GSS, foreign = ifelse(reg16 == 0, 1, 0)) 
GSS <- mutate(GSS, new_england = ifelse(reg16 == 1, 1, 0))
GSS <- mutate(GSS, middle_atlantic = ifelse(reg16 == 2, 1, 0))
GSS <- mutate(GSS, east_north_central = ifelse(reg16 == 3, 1 , 0))
GSS <- mutate(GSS, west_north_central = ifelse(reg16 == 4, 1, 0))
GSS <- mutate(GSS, south_atlantic = ifelse(reg16 == 5, 1, 0))
GSS <- mutate(GSS, east_south_atlantic = ifelse(reg16 == 6, 1, 0))
GSS <- mutate(GSS, west_south_central = ifelse(reg16 == 7, 1, 0))
GSS <- mutate(GSS, mountain = ifelse(reg16 == 8, 1, 0))
GSS <- mutate(GSS, pacific = ifelse(reg16 == 9, 1, 0))


table(GSS$foreign)
table(GSS$new_england)
table(GSS$middle_atlantic)
table(GSS$east_north_central)
table(GSS$west_north_central)
table(GSS$south_atlantic)
table(GSS$east_south_atlantic)
table(GSS$west_south_central)
table(GSS$mountain)
table(GSS$pacific)

# STEP 3: Confirm creation (if necessary)

table(GSS$reg16, GSS$foreign)
table(GSS$reg16, GSS$new_england)
table(GSS$reg16, GSS$middle_atlantic)
table(GSS$reg16, GSS$east_north_central)
table(GSS$reg16, GSS$west_north_central)
table(GSS$reg16, GSS$south_atlantic)
table(GSS$reg16, GSS$east_south_atlantic)
table(GSS$reg16, GSS$west_south_central)
table(GSS$reg16, GSS$mountain)
table(GSS$reg16, GSS$pacific)
        

############                  INDEPENDENT VARIABLE                    ############
############                   Trust in Government                     ############
        
# STEP 1: Examine variable and coding schema 

table(GSS$natcrime)

# STEP 2: Dummy variables for each answer

GSS <- mutate(GSS, too_little = ifelse(natcrime == 1, 1, 0))
GSS <- mutate(GSS, about_right = ifelse(natcrime == 2, 1, 0))
GSS <- mutate(GSS, too_much = ifelse(natcrime == 3, 1, 0))

table(GSS$too_little)
table(GSS$about_right)
table(GSS$too_much)

# STEP 3: Confirm creation (if necessary)
table(GSS$natcrime, GSS$too_little)
table(GSS$natcrime, GSS$about_right)
table(GSS$natcrime, GSS$too_much)


############                  INDEPENDENT VARIABLE                    ############
############             Afraid to Walk at Night in Neighborhood      ############

# STEP 1: Examine variable and coding schema 

table(GSS$fear)

# STEP 2: Dummy variables for each answer

GSS <- mutate(GSS, yes_fear = ifelse(fear == 1, 1, 0))
GSS <- mutate(GSS, no_fear = ifelse(fear == 2, 1, 0))

table(GSS$yes_fear)
table(GSS$no_fear)

# STEP 3: Confirm creation (if necessary)
table(GSS$fear, GSS$yes_fear)
      
table(GSS$fear, GSS$no_fear)



####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("favor_gun", "oppose_gun", "has_gun", "no_gun", "refused", 
                "gunlaw", "owngun", "strong_democrat", "low_democrat", "liberal_independent", "mid_independent",
                "conservative_independent", "low_republican", "strong_republican", "other_party", "foreign", "new_england", 
                "middle_atlantic", "east_north_central", "west_north_central", "south_atlantic", "east_south_atlantic",
                "west_south_central", "mountain", "pacific", "too_little", "about_right", "too_much", "yes_fear", "no_fear")
               
### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))


### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: Use describe command for marginal probabilities in Table 1
describe(my_dataset)


####################################################################################
############              PHASE 4: Correlation Matrix                  ############
####################################################################################
#Correlation between key IV and key DV 
cor(my_dataset$owngun, my_dataset$gunlaw)

cor(my_dataset)




####################################################################################
############              PHASE 5: Logistic Regression Analysis         ############
####################################################################################

model1a <- glm(favor_gun ~ has_gun + no_gun + refused, data = my_dataset, family = binomial)

summary(model1a)

r2_model1a <- 1 - (1467.2 / 1492.1)


model2a <- glm(favor_gun ~ has_gun + no_gun + refused + foreign + new_england + middle_atlantic +
                 east_north_central + west_north_central + south_atlantic + east_south_atlantic + 
                 west_south_central + mountain + pacific, data = my_dataset, family = binomial)

summary(model2a)

r2_model2a <- 1 - (1397.8 / 1492.1)


model3a <- glm(favor_gun ~ has_gun + no_gun + refused + foreign + new_england + middle_atlantic +
                 east_north_central + west_north_central + south_atlantic + east_south_atlantic + 
                 west_south_central + mountain + pacific + strong_democrat +
                 low_democrat + liberal_independent + mid_independent + conservative_independent +
                 low_republican + strong_republican + other_party, data = my_dataset, family = binomial)


summary(model3a)

r2_model3a <- 1 - (1316.8 / 1492.1)


model4a <- glm(favor_gun ~ has_gun + no_gun + refused + foreign + new_england + middle_atlantic +
                 east_north_central + west_north_central + south_atlantic + east_south_atlantic + 
                 west_south_central + mountain + pacific + strong_democrat +
                 low_democrat + liberal_independent + mid_independent + conservative_independent +
                 low_republican + strong_republican + other_party + about_right + too_much + too_little + 
                 yes_fear + no_fear, data = my_dataset, family = binomial)


summary(model4a)

r2_model4a <- 1 - (1297.8 / 1492.1)




