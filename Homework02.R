getwd()

# Activity2  --------------------------------------------------------------


data <- read.csv("lab2.csv")
library(tidyverse)
library(ggplot2)

data %>%
  gather('base_sport', 'base_pain', 'base_qol', 'first_sport', 'first_pain', 'first_qol',
         'second_sport', 'second_pain', 'second_qol', key = "time_category", value = "value") -> data1
data1 %>%
  separate(time_category, into = c("time", "category")) -> data2

data2$time <- recode(data2$time, 'base' = "base",
                     'first' = "one year",
                     'second' = "two years")

data2 %>%
  group_by(time, category) %>%
  summarise(mean = mean(value), standard_deviation = sd(value)) %>%
  ggplot(aes(x = time, y = mean, group = category, color = category)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean - standard_deviation, ymax = mean + standard_deviation, width = 0.1))


# Assignment2 -------------------------------------------------------------


data1 <- read.csv("coverage.csv", skip = 2)
data2 <- read.csv("expenditures.csv", skip = 2)
data1 <- data1[1:52,]
data2 <- data2[1:52,c(1,24,25)]


library(tidyverse)
library(ggplot2)
library(reshape)

data1 <- rename(data1, c(X2013__Non.Group = "X2013__NonGroup",
                       X2014__Non.Group = "X2014__NonGroup",
                       X2015__Non.Group = "X2015__NonGroup",
                       X2016__Non.Group = "X2016__NonGroup",
                       X2013__Other.Public = "X2013__OtherPublic",
                       X2014__Other.Public = "X2014__OtherPublic",
                       X2015__Other.Public = "X2015__OtherPublic",
                       X2016__Other.Public = "X2016__OtherPublic"))

data2 <- rename(data2, c(X2013__Total.Health.Spending = "X2013__TotalHealthSpending",
                         X2014__Total.Health.Spending = "X2014__TotalHealthSpending"))

data1 %>%
  gather('X2013__Employer', 'X2013__NonGroup', 'X2013__Medicaid', 'X2013__Medicare', 'X2013__OtherPublic', 'X2013__Uninsured', 'X2013__Total',
         'X2014__Employer', 'X2014__NonGroup', 'X2014__Medicaid', 'X2014__Medicare', 'X2014__OtherPublic', 'X2014__Uninsured', 'X2014__Total',
         'X2015__Employer', 'X2015__NonGroup', 'X2015__Medicaid', 'X2015__Medicare', 'X2015__OtherPublic', 'X2015__Uninsured', 'X2015__Total',
         'X2016__Employer', 'X2016__NonGroup', 'X2016__Medicaid', 'X2016__Medicare', 'X2016__OtherPublic', 'X2016__Uninsured', 'X2016__Total',
         key = "year_coverage", value = "number") -> data3

data2 %>%
  gather('X2013__TotalHealthSpending', 'X2014__TotalHealthSpending',
         key = "year_expenditures", value = "number") -> data4

data3 %>%
  separate(year_coverage, into = c("year", "Category")) -> data5

data4 %>%
  separate(year_expenditures, into = c("year", "Category")) -> data6

data5$year <- recode(data5$year, 'X2013' = "2013",
                     'X2014' = "2014",
                     'X2015' = "2015",
                     'X2016' = "2016")
data6$year <- recode(data6$year, 'X2013' = "2013",
                     'X2014' = "2014")

data_sum <- rbind(data5, data6)




