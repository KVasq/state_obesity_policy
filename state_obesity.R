library(tidyverse)
library(haven)
library(hash)

policy_data <- read.csv("Child-Nutrition-Data-2021.csv")
colnames(policy_data)[1] <- "Title"

child017_data <- read_dta("nsch_2019_screener.dta")
child_overweight <- read_dta("nsch_2019_topical.dta") %>% summarise(fipsst, overweight, bmiclass) %>% na.omit()

fips <- c(1:2,4:6,8:10,12:13,15:42,44:51,53:56)
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
            "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
            "TX","UT","VT","VA","WA","WV","WI","WY")
state_fips <- hash(keys=states, values=fips)

# find the percentage of obese and overweight children in each state
obesity_pct <- c()
for (s in states) {
  obesity_pct <- c(obesity_pct, length(which(child_overweight[child_overweight$fipsst == state_fips[[s]],]$bmiclass == 4)) / 
    nrow(child_overweight[child_overweight$fipsst == state_fips[[s]],]))
}
obesity_pct <- obesity_pct * 100

overweight_pct <- c()
for (s in states) {
  overweight_pct <- c(overweight_pct, length(which(child_overweight[child_overweight$fipsst == state_fips[[s]],]$bmiclass == 3)) / 
                     nrow(child_overweight[child_overweight$fipsst == state_fips[[s]],]))
}
overweight_pct <- overweight_pct * 100


state_overweight <- data.frame(FIPS=fips, STATE=states, OBESITY_PCT=obesity_pct, OVRWEIGHT_PCT=overweight_pct)
