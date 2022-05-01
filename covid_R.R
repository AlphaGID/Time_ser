rm(list=ls()) ####removes all variables stored previously

library(Hmisc)
library(readr)

Df<- read_csv("C:\\Users\\LENOVO USER\\Downloads\\COVID19_line_list_data.csv")
Df

describe(Df)

#######Data Cleaning

Df$death_dummy <- as.integer(Df$death !=0)
unique(Df$death_dummy)

#Death rate 
sum(Df$death_dummy) / nrow(Df)

#######Age

### H0: prople who die are older
alive = subset(Df, death_dummy == 0)
dead = subset(Df, death_dummy == 1)
mean(alive$age, na.rm = TRUE)
mean(dead$age, na.rm = TRUE)
t.test(alive$age, dead$age, alternative = "two.sided", conf.levelv = 0.95)
## normally, if p-value < 0.05, we reject the null hypothesis 
## here we see that p value 2.2e-16 = 0, hence we reject the null hypothesis and conclude that this is statistically significant


###### Gender
### H0: gender has no effect
men = subset(Df, gender == "male")
women = subset(Df, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.levelv = 0.95)
## here we see that p value 0.002105, hence conclude that this is statistically significant