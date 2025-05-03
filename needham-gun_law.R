
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

# STEP 3: Confirm creation (if necessary)
table(GSS$owngun, GSS$has_gun)
table(GSS$owngun, GSS$no_gun)



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

############                  INDEPENDENT VARIABLE                    ############
############               Respondent's Political Identity            ############

# STEP 1: Examine variable and coding schema
table(GSS$partyid)

# STEP 2: Create 3-group political identity variable

GSS <- GSS %>%
  mutate(
    party_group = case_when(
      partyid %in% c(0, 1) ~ "Democrat",
      partyid %in% c(2, 3, 4) ~ "Independent",
      partyid %in% c(5, 6) ~ "Republican",
      partyid == 7 ~ "Other Party",
      TRUE ~ NA_character_
    ),
    democrat = ifelse(party_group == "Democrat", 1, 0),
    independent = ifelse(party_group == "Independent", 1, 0),
    republican = ifelse(party_group == "Republican", 1, 0),
    other_party = ifelse(party_group == "Other Party", 1, 0)
  )

table(GSS$democrat)
table(GSS$independent)
table(GSS$republican)
table(GSS$other_party)

# STEP 3: Confirm coding with cross-tab (optional, like before)
table(GSS$partyid, GSS$party_group)


############                  INDEPENDENT VARIABLE                    ############
############                        Region                             ############

# STEP 1: Examine variable and coding schema 
table(GSS$reg16)

# STEP 2: Create dummy variables for broader regions

GSS <- GSS %>%
  mutate(
    foreign = ifelse(reg16 == 0, 1, 0),
    northeast = ifelse(reg16 %in% c(1, 2), 1, 0),           # New England + Middle Atlantic
    midwest = ifelse(reg16 %in% c(3, 4), 1, 0),             # East North Central + West North Central
    south = ifelse(reg16 %in% c(5, 6, 7), 1, 0),            # South Atlantic + East/West South Central
    west = ifelse(reg16 %in% c(8, 9), 1, 0)                 # Mountain + Pacific
  )

# STEP 3: Verify dummy variables created
table(GSS$foreign)
table(GSS$northeast)
table(GSS$midwest)
table(GSS$south)
table(GSS$west)

table(GSS$foreign)
prop.table(table(GSS$foreign))


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

my_varlist <- c("favor_gun", "oppose_gun", "has_gun", "no_gun", 
                "gunlaw", "owngun", "democrat", "republican", "independent", "other_party", "foreign",
                "northeast", "midwest", "south", "west", "too_little", "about_right", "too_much", "yes_fear", "no_fear")


               
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

model1a <- glm(favor_gun ~ has_gun + no_gun, data = my_dataset, family = binomial)

summary(model1a)


model2a <- glm(favor_gun ~ has_gun + no_gun + foreign + northeast + midwest + south + west, data = my_dataset, family = binomial)


summary(model2a)

model3a <- glm(favor_gun ~ has_gun + no_gun  +  foreign + northeast + midwest + south + west + democrat + republican 
               + independent + other_party, data = my_dataset, family = binomial)

summary(model3a)



model4a <- glm(favor_gun ~ has_gun + no_gun + democrat + independent + republican 
                + other_party + foreign + northeast + midwest + south + west + about_right + 
                 too_much + too_little +  yes_fear + no_fear, data = my_dataset, family = binomial)


summary(model4a)





