library(tidyverse)
library(haven)
library(hash)
library(usmap)
library(readxl)

policy_data <- read.csv("Child-Nutrition-Data-2021 - Sheet1.csv")



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


state_overweight <- data.frame(FIPS=fips, state=states, OBESITY_PCT=obesity_pct, OVRWEIGHT_PCT=overweight_pct)

#regression methods (policy implementation vs obesity each state)


policy_data <- policy_data %>% slice(match(State,states))
data <- merge(policy_data, state_overweight, by.x = "State", by.y = "state")

data$SNAP.Eligibility <- as.numeric(sub("%","",data$SNAP.Eligibility))
summary(lm(OBESITY_PCT ~ SNAP.Eligibility, data = data))
scatter.smooth(x=data$SNAP.Eligibility, y=data$OBESITY_PCT, main="Obesity ~ Participation")

data$SNAP.Participation.Children.1 <- as.numeric(sub("%","",data$SNAP.Participation.Children.1)) 
summary(lm(OBESITY_PCT ~ SNAP.Participation.Children.1, data = data))
scatter.smooth(x=data$SNAP.Participation.Children.1, y=data$OBESITY_PCT, main="Obesity ~ Participation")


data$Head.Start.1 <- as.numeric(sub("%","",data$Head.Start.1)) 
summary(lm(OBESITY_PCT ~ Head.Start.1, data = data))
scatter.smooth(x=data$Head.Start.1, y=data$OBESITY_PCT, main="Obesity ~ Participation")

data$Women..Infants..and.Children.1 <- as.numeric(sub("%","",data$Women..Infants..and.Children.1)) 
summary(lm(OBESITY_PCT ~ Women..Infants..and.Children.1, data = data))
scatter.smooth(x=data$Women..Infants..and.Children.1, y=data$OBESITY_PCT, main="Obesity ~ Participation")


data$National.School.Lunch.Program.1 <- as.numeric(sub("%","",data$National.School.Lunch.Program.1)) 
summary(lm(OBESITY_PCT ~ National.School.Lunch.Program.1, data = data))
scatter.smooth(x=data$National.School.Lunch.Program.1, y=data$OBESITY_PCT, main="Obesity vs National School Lunch Program Participation")

#graph obesity rates onto us map
colnames(data)[1] <- "state"
plot_usmap(data = data, values = "OBESITY_PCT", regions = "states") +
  labs(title = "Obesity Rate per State") +
  scale_fill_continuous(name = "Obesity Rate (%)", low = "white", high = "red") +
  theme(panel.background = element_blank())

plot_usmap(data = data, values = "National.School.Lunch.Program.1", regions = "states") +
  labs(title = "National School Lunch Program Implementation Percentage per State") +
  scale_fill_continuous(name = "Lunch Program Eligibility (%)") +
  theme(panel.background = element_blank())

#find connection between school lunch program and educational attainment
edu_attainment <- read.csv("education.csv") %>% subset(State != "District of Columbia")
edu_attainment$State <- states

data <- merge(data, edu_attainment, by.x = "state", by.y = "State")

summary(lm(hs_pct ~ National.School.Lunch.Program.1, data = data))
scatter.smooth(x=data$National.School.Lunch.Program.1, y=data$hs_pct, main="High School Education Attainment vs National School Lunch Program Eligibility")
