#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V 
# Author: Xi Zheng(1005153628)
# Data: 20 December 2020
# Contact: xi.zheng@mail.utoronto.ca


#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)

raw_data <- read_dta("2019.dta")

#Select variables we want
reduced_data <- 
  raw_data %>% select(cps19_gender, cps19_marital, cps19_employment, cps19_province, cps19_education, cps19_income_cat, cps19_votechoice, cps19_votechoice_pr, cps19_vote_lean_pr, cps19_vote_lean, cps19_vote_unlike_pr)
reduced_data[is.na(reduced_data)] <- 0

##change the gender data type to the same format as Census data
MF <- c(1,2)
reduced_data <- reduced_data[(reduced_data$cps19_gender %in% MF),]
reduced_data <- 
  reduced_data %>%
  mutate(gender = case_when(cps19_gender == 1 ~"male",
                         cps19_gender == 2 ~"female"))

## change the employment data type
vaildJob <- c(1,2,3,4,5,6,7,8,9)
reduced_data <- reduced_data[(reduced_data$cps19_employment %in% vaildJob),]
seta <- c(1,2,3)
setb <-c(7, 10)
setc <- c(9,10,11,12,13)
reduced_data <- 
  reduced_data %>%
  mutate(employment = case_when(cps19_employment %in% seta  ~"Working at a paid job or self-employed",
                             cps19_employment == 5 ~"Looking for paid work",
                             cps19_employment == 6 ~"Going to school",
                             cps19_employment %in% setb ~"Household work",
                             cps19_employment == 4 ~"Retired",
                             cps19_employment == 8 ~ "Long term illness",
                             cps19_employment %in% setc ~ "Other"))

## change the marital data type
vaildType <- c(1,2,3,4,5,6)
reduced_data <- reduced_data[(reduced_data$cps19_marital %in% vaildType),]
reduced_data <- 
  reduced_data %>%
  mutate(marital = case_when(cps19_marital == 1 ~"Married",
                             cps19_marital == 2 ~"Living common-law",
                             cps19_marital == 3 ~"Divorced",
                             cps19_marital == 4 ~"Separated",
                             cps19_marital == 5 ~"Widowed",
                             cps19_marital == 6 ~ "Single, never married"))

## change the eduaction level to the same format as Census
vaildAnswer <- c(1,2,3,4,5,6,7,8,9,10,11)
reduced_data <- reduced_data[(reduced_data$cps19_education %in% vaildAnswer),]
set1 <- c(1,2,3,4)
set2 <-c(10,11)
reduced_data <- 
  reduced_data %>%
  mutate(edu = case_when(cps19_education %in% set1 ~"Less than high school diploma or its equivalent",
                         cps19_education == 5 ~"High school diploma or high school equivalency certificate",
                         cps19_education == 6 ~"Trade certificate or diploma",
                         cps19_education == 7 ~"College, CEGEP or other non-university certificate or di. . .",
                         cps19_education == 8 ~"University certificate or diploma below the bachelor’s level",
                         cps19_education == 9 ~ "Bachelor’s degree (e.g. B.A., B.Sc., LL.B.)",
                         cps19_education %in% set2 ~"University certificate, diploma or degree above the bach. . ."))

## change the province data type
vaildPro <- c(14,15,16,17,18,20,22,23,24,25)
reduced_data <- reduced_data[(reduced_data$cps19_province %in% vaildPro),]
reduced_data <- 
  reduced_data %>%
  mutate(province = case_when(cps19_province == 18 ~"Newfoundland and Labroador",
                             cps19_province == 23 ~"Prince Edward Island",
                             cps19_province == 20 ~"Nova Scotia",
                             cps19_province == 17 ~"New Brunswick",
                             cps19_province == 24 ~"Quebec",
                             cps19_province == 22 ~ "Ontario",
                             cps19_province == 16 ~"Manitoba",
                             cps19_province == 25 ~"Saskatchewan",
                             cps19_province == 14 ~"Alberta",
                             cps19_province == 15 ~"British Columbia"))

## change the income data type
vaildIn <- c(1,2,3,4,5,6,7,8)
reduced_data <- reduced_data[(reduced_data$cps19_income_cat %in% vaildIn),]
set11 <- c(1,2,3)
set22 <-c(4,5)
set33 <- c(6,7,8)
reduced_data <- 
  reduced_data %>%
  mutate(income = case_when(cps19_income_cat %in% set11  ~"Low family Income: below $60,000",
                            cps19_income_cat %in% set22 ~"Middle family Income: $60,000 - $10,000",
                            cps19_income_cat %in% set33 ~ "High family Income: above $10,000"))

## get the vote intention for the respondent
reduced_data<-
  reduced_data %>% mutate(vote = cps19_votechoice + cps19_votechoice_pr + cps19_vote_lean_pr + cps19_vote_lean + cps19_vote_unlike_pr)
reduced_data<-
  reduced_data %>%
  mutate(voteLiberal = 
           ifelse(vote==1, 1, 0))
reduced_data<-
  reduced_data %>%
  mutate(voteCon = 
           ifelse(vote==2, 1, 0))
write_csv(reduced_data, "ces2019.csv")

