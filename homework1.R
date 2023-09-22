#Team members - Mishal Nawaz, Vanessa Crowe and Akash Prasaud

x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)

fm <- lm(y ~ x)
summary(fm)

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))

load("Household_Pulse_data_w57.RData")
\Household_Pulse_data[1:10,1:6]
\attach(Household_Pulse_data)

attach(Household_Pulse_data)
summary(Household_Pulse_data)

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])

Average ages of men and women
mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])

mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
[1] 1971.481
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
[1] 15.61139
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
[1] 1970.25
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
[1] 16.17685

#intersting fact
#it was very intersting to see how the data that we have was not very diverse as most of the people were white though it could be because the place where data was collected, it was not diverse.