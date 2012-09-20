# TraMineR course assignment 2

#Load biofam data

data(biofam)

names(biofam)                   # Print the variable names
str(biofam)                     # Look at the data structure, variable types


####### Create new analysis variables

summary(biofam$birthyr)         # Look at the distribution of raw data
hist(biofam$birthyr)


  # Create new variables

age <- 2002 - biofam$birthyr
summary(age)

cohort <- cut(biofam$birthyr, breaks=c(1900,1930,1940,1950,1960), 
              labels=c("1900-1929","1930-1939","1940-1949","1950-1959"), right=FALSE)
table(cohort)

mwc <- as.numeric(biofam$a25==6)   # Married with children @ 25 y, yes/no for logistic regressions
table(mwc)
ct2 <- table(biofam$a25, mwc)
ct2

biofam2 <- cbind(biofam, age, cohort, mwc)     # Merge the new variables with the dataframe


# Subset the data

        # Look at the distribution of sex
levels(biofam$sex)            
table(biofam$sex)

woman <- subset(biofam2, sex == "woman")            # Subset the data for women only



####### End of data steps



# Run univariate descriptive statistics

summary(biofam2$age)

summary(woman$age)

hist(biofam2$birthyr, breaks=c(1900,1930,1940,1950,1960), freq=TRUE)

table (biofam2$cohort)         # Frequency table of Cohort factor



# Crosstabulate Cohort with state at age 25

levels(biofam2$a25) <- c("Living with parents","Left home","Married",
                         "Left home, married","Having children","Left home, having children",
                         "Left home, married with children","Divorced")
ct1 <- table(biofam2$cohort, biofam2$a25)
ct1

prop.table(ct1, 1)    # Row %
prop.table(ct1, 2)    # Column %
margin.table(ct1, 1)  # Row totals
margin.table(ct1, 2)  # Column totals


# Logistic regression

lg <- glm(mwc ~ plingu02 + sex, family=binomial, data=biofam2)
summary(lg)
exp(lg$coefficients)
lg.coeff <- as.data.frame(summary(lg)$coefficients)
lg.coeff <- cbind(lg.coeff, `OR`=exp(lg.coeff[,"Estimate"]))
lg.coeff

lg.y <- glm(mwc ~ plingu02 + sex, family=binomial, data=biofam2, 
            subset=(biofam2$cohor=="1950-1959"))
summary(lg.y)
exp(lg.y$coefficients)
lg.y.coeff <- as.data.frame(summary(lg.y)$coefficients)
lg.y.coeff <- cbind(lg.y.coeff, `OR`=exp(lg.y.coeff[,"Estimate"]))
lg.y.coeff