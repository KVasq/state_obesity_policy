library(tidyverse)

policy_data <- read.csv("Child-Nutrition-Data-2021.csv")
colnames(policy_data)[1] <- "Title"

child017_data <- read_dta("nsch_2019_screener.dta")