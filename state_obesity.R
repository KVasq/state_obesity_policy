library(tidyverse)
library(haven)
library(hash)

policy_data <- read.csv("Child-Nutrition-Data-2021.csv")
colnames(policy_data)[1] <- "Title"

child017_data <- read_dta("nsch_2019_screener.dta")

fips <- c(1:2,4:6,8:10,12:13,15:42,44:51,53:56)
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
            "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
            "TX","UT","VT","VA","WA","WV","WI","WY")
state_fips <- hash(keys=states, values=fips)


