#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/cgi-bin/sda/hsda?harcsda4+gss30
#(2016 General social survey on Canadians at Work and Home)
# Author: Xi Zheng(1005153628)
# Data: 20 December 2020
# Contact: xi.zheng@mail.utoronto.ca

#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)

raw_data <- read.csv("AAeC6ZbA.csv")
reduced_data <- 
  raw_data %>% select(sex, marstat, ehg3_01, mar_110, prv, famincg2)

reduced_data <- 
  reduced_data %>%
  mutate(gender = case_when(sex == 1 ~"male",
                         sex == 2 ~"female"))

reduced_data <- 
  reduced_data %>%
  mutate(marital = case_when(marstat == 1 ~"Married",
                             marstat == 2 ~"Living common-law",
                             marstat == 5 ~"Divorced",
                             marstat == 4 ~"Separated",
                             marstat == 3 ~"Widowed",
                             marstat == 6 ~ "Single, never married"))

vaildAnswer <- c(1,2,3,4,5,6,7)
reduced_data <- reduced_data[(reduced_data$ehg3_01 %in% vaildAnswer),]
reduced_data <- 
  reduced_data %>%
  mutate(edu = case_when(ehg3_01 == 1 ~"Less than high school diploma or its equivalent",
                         ehg3_01 == 2 ~"High school diploma or high school equivalency certificate",
                         ehg3_01 == 3 ~"Trade certificate or diploma",
                         ehg3_01 == 4 ~"College, CEGEP or other non-university certificate or di. . .",
                         ehg3_01 == 5 ~"University certificate or diploma below the bachelor’s level",
                         ehg3_01 == 6 ~ "Bachelor’s degree (e.g. B.A., B.Sc., LL.B.)",
                         ehg3_01 == 7 ~"University certificate, diploma or degree above the bach. . ."))

## change the employment data type
vaildJob <- c(1,2,3,4,5,6,7,8,9, 10)
reduced_data <- reduced_data[(reduced_data$mar_110 %in% vaildType),]
seta <- c(1,2,3)
setb <-c(4, 5)
setc <- c(7,9,10)
reduced_data <- 
  reduced_data %>%
  mutate(employment = case_when(mar_110 == 1  ~"Working at a paid job or self-employed",
                                mar_110 == 2 ~"Looking for paid work",
                                mar_110 == 3 ~"Going to school",
                                mar_110 %in% setb ~"Household work",
                                mar_110 == 6 ~"Retired",
                                mar_110 == 8 ~ "Long term illness",
                                mar_110 %in% setc ~ "Other"))
## change the province data type
reduced_data <- 
  reduced_data %>%
  mutate(province = case_when(prv == 10 ~"Newfoundland and Labroador",
                              prv == 11 ~"Prince Edward Island",
                              prv == 12 ~"Nova Scotia",
                              prv == 13 ~"New Brunswick",
                              prv == 24 ~"Quebec",
                              prv == 35 ~ "Ontario",
                              prv == 46 ~"Manitoba",
                              prv == 47 ~"Saskatchewan",
                              prv == 48 ~"Alberta",
                              prv == 59 ~"British Columbia"))

## change the income data type
set11 <- c(1,2,3)
set22 <-c(4)
set33 <- c(5,6)
reduced_data <- 
  reduced_data %>%
  mutate(income = case_when(famincg2 %in% set11  ~"Low family Income: below $60,000",
                            famincg2 %in% set22 ~"Middle family Income: $60,000 - $10,000",
                            famincg2 %in% set33 ~ "High family Income: above $10,000"))


write_csv(reduced_data, "gss2017.csv")



