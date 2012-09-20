> # TraMineR course assignment 2
> 
> #Load biofam data
> 
> data(biofam)
> 
> names(biofam)                   # Print the variable names
 [1] "idhous"   "sex"      "birthyr"  "nat_1_02" "plingu02" "p02r01"   "p02r04"   "cspfaj"   "cspmoj"  
[10] "a15"      "a16"      "a17"      "a18"      "a19"      "a20"      "a21"      "a22"      "a23"     
[19] "a24"      "a25"      "a26"      "a27"      "a28"      "a29"      "a30"      "wp00tbgp" "wp00tbgs"
> str(biofam)                     # Look at the data structure, variable types
'data.frame':  2000 obs. of  27 variables:
 $ idhous  : num  66891 28621 57711 17501 147701 ...
 $ sex     : Factor w/ 2 levels "man","woman": 1 1 2 1 1 1 1 1 1 2 ...
 $ birthyr : num  1943 1935 1946 1918 1946 ...
 $ nat_1_02: Factor w/ 200 levels "other error",..: 6 6 6 6 6 6 6 6 6 6 ...
 $ plingu02: Factor w/ 3 levels "french","german",..: 2 2 1 2 2 3 2 1 1 2 ...
 $ p02r01  : Factor w/ 13 levels "other error",..: 6 7 13 7 7 7 6 9 6 7 ...
 $ p02r04  : Factor w/ 14 levels "other error",..: 9 13 7 13 7 6 7 14 9 13 ...
 $ cspfaj  : Factor w/ 12 levels "active occupied but not classified",..: 7 7 7 5 NA 12 NA 11 7 7 ...
 $ cspmoj  : Factor w/ 12 levels "active occupied but not classified",..: 7 NA 9 NA NA NA NA NA 7 NA ...
 $ a15     : num  0 0 0 0 0 0 0 0 0 1 ...
 $ a16     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a17     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a18     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a19     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a20     : num  0 1 0 1 1 0 0 0 0 1 ...
 $ a21     : num  0 1 0 1 1 0 0 1 0 1 ...
 $ a22     : num  0 1 1 1 1 0 0 1 0 1 ...
 $ a23     : num  0 1 1 1 1 0 0 1 0 1 ...
 $ a24     : num  3 1 1 1 1 0 2 1 0 6 ...
 $ a25     : num  6 1 1 1 1 0 2 1 0 6 ...
 $ a26     : num  6 3 1 1 1 0 2 3 6 6 ...
 $ a27     : num  6 6 3 1 1 0 2 3 6 6 ...
 $ a28     : num  6 6 6 1 6 0 2 3 6 6 ...
 $ a29     : num  6 6 6 1 6 0 2 6 6 6 ...
 $ a30     : num  6 6 6 1 6 0 2 6 6 6 ...
 $ wp00tbgp: num  1053 855 575 1527 796 ...
 $ wp00tbgs: num  0.935 0.759 0.51 1.356 0.707 ...
> 
> 
> ####### Create new analysis variables
> 
> summary(biofam$birthyr)         # Look at the distribution of raw data
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1909    1935    1944    1943    1951    1957 
> hist(biofam$birthyr)
> 
> 
>   # Create new variables
> 
> age <- 2002 - biofam$birthyr
> summary(age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  45.00   51.00   58.00   59.47   67.00   93.00 
> 
> cohort <- cut(biofam$birthyr, breaks=c(1900,1930,1940,1950,1960), 
+               labels=c("1900-1929","1930-1939","1940-1949","1950-1959"), right=FALSE)
> table(cohort)
cohort
1900-1929 1930-1939 1940-1949 1950-1959 
      260       466       632       642 
> 
> mwc <- as.numeric(biofam$a25==6)   # Married with children @ 25 y, yes/no for logistic regressions
> table(mwc)
mwc
   0    1 
1586  414 
> ct2 <- table(biofam$a25, mwc)
> ct2
   mwc
      0   1
  0 509   0
  1 522   0
  2 146   0
  3 371   0
  4   2   0
  5  12   0
  6   0 414
  7  24   0
> 
> biofam2 <- cbind(biofam, age, cohort, mwc)     # Merge the new variables with the dataframe
> 
> 
> # Subset the data
> 
>         # Look at the distribution of sex
> levels(biofam$sex)            
[1] "man"   "woman"
> table(biofam$sex)

  man woman 
  908  1092 
> 
> woman <- subset(biofam2, sex == "woman")            # Subset the data for women only
> 
> 
> 
> ####### End of data steps
> 
> 
> 
> # Run univariate descriptive statistics
> 
> summary(biofam2$age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  45.00   51.00   58.00   59.47   67.00   93.00 
> 
> summary(woman$age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  45.00   50.00   57.00   59.39   66.00   93.00 
> 
> hist(biofam2$birthyr, breaks=c(1900,1930,1940,1950,1960), freq=TRUE)
Warning message:
In plot.histogram(r, freq = freq1, col = col, border = border, angle = angle,  :
  the AREAS in the plot are wrong -- rather use freq=FALSE
> 
> table (biofam2$cohort)         # Frequency table of Cohort factor

1900-1929 1930-1939 1940-1949 1950-1959 
      260       466       632       642 
> 
> 
> 
> # Crosstabulate Cohort with state at age 25
> 
> levels(biofam2$a25) <- c("Living with parents","Left home","Married",
+                          "Left home, married","Having children","Left home, having children",
+                          "Left home, married with children","Divorced")
> ct1 <- table(biofam2$cohort, biofam2$a25)
> ct1
           
              0   1   2   3   4   5   6   7
  1900-1929 120  41  31  30   0   1  33   4
  1930-1939 140  91  46  87   0   2  98   2
  1940-1949 145 134  42 137   1   4 160   9
  1950-1959 104 256  27 117   1   5 123   9
> 
> prop.table(ct1, 1)    # Row %
           
                      0           1           2           3           4           5           6           7
  1900-1929 0.461538462 0.157692308 0.119230769 0.115384615 0.000000000 0.003846154 0.126923077 0.015384615
  1930-1939 0.300429185 0.195278970 0.098712446 0.186695279 0.000000000 0.004291845 0.210300429 0.004291845
  1940-1949 0.229430380 0.212025316 0.066455696 0.216772152 0.001582278 0.006329114 0.253164557 0.014240506
  1950-1959 0.161993769 0.398753894 0.042056075 0.182242991 0.001557632 0.007788162 0.191588785 0.014018692
> prop.table(ct1, 2)    # Column %
           
                     0          1          2          3          4          5          6          7
  1900-1929 0.23575639 0.07854406 0.21232877 0.08086253 0.00000000 0.08333333 0.07971014 0.16666667
  1930-1939 0.27504912 0.17432950 0.31506849 0.23450135 0.00000000 0.16666667 0.23671498 0.08333333
  1940-1949 0.28487230 0.25670498 0.28767123 0.36927224 0.50000000 0.33333333 0.38647343 0.37500000
  1950-1959 0.20432220 0.49042146 0.18493151 0.31536388 0.50000000 0.41666667 0.29710145 0.37500000
> margin.table(ct1, 1)  # Row totals

1900-1929 1930-1939 1940-1949 1950-1959 
      260       466       632       642 
> margin.table(ct1, 2)  # Column totals

  0   1   2   3   4   5   6   7 
509 522 146 371   2  12 414  24 
> 
> 
> # Logistic regression
> 
> lg <- glm(mwc ~ plingu02 + sex, family=binomial, data=biofam2)
> summary(lg)

Call:
glm(formula = mwc ~ plingu02 + sex, family = binomial, data = biofam2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.8282  -0.7602  -0.5666  -0.5414   2.2714  

Coefficients:
                Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -1.5486     0.1404 -11.031  < 2e-16 ***
plingu02german   -0.1996     0.1349  -1.480   0.1389    
plingu02italian  -0.9523     0.3723  -2.558   0.0105 *  
sexwoman          0.6547     0.1284   5.100  3.4e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1693.3  on 1646  degrees of freedom
Residual deviance: 1657.8  on 1643  degrees of freedom
  (353 observations deleted due to missingness)
AIC: 1665.8

Number of Fisher Scoring iterations: 4

> exp(lg$coefficients)
    (Intercept)  plingu02german plingu02italian        sexwoman 
      0.2125522       0.8190783       0.3858666       1.9246089 
> lg.coeff <- as.data.frame(summary(lg)$coefficients)
> lg.coeff <- cbind(lg.coeff, `OR`=exp(lg.coeff[,"Estimate"]))
> lg.coeff
                  Estimate Std. Error    z value     Pr(>|z|)        OR
(Intercept)     -1.5485676  0.1403873 -11.030681 2.717989e-28 0.2125522
plingu02german  -0.1995755  0.1348644  -1.479823 1.389204e-01 0.8190783
plingu02italian -0.9522635  0.3723226  -2.557630 1.053882e-02 0.3858666
sexwoman         0.6547228  0.1283838   5.099729 3.401396e-07 1.9246089
> 
> lg.y <- glm(mwc ~ plingu02 + sex, family=binomial, data=biofam2, 
+             subset=(biofam2$cohor=="1950-1959"))
> summary(lg.y)

Call:
glm(formula = mwc ~ plingu02 + sex, family = binomial, data = biofam2, 
    subset = (biofam2$cohor == "1950-1959"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.8801  -0.6945  -0.5879  -0.4713   2.1226  

Coefficients:
                Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -1.66780    0.25193  -6.620 3.59e-11 ***
plingu02german  -0.47379    0.24324  -1.948 0.051437 .  
plingu02italian  0.07701    0.48716   0.158 0.874400    
sexwoman         0.84219    0.24085   3.497 0.000471 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 520.85  on 533  degrees of freedom
Residual deviance: 503.57  on 530  degrees of freedom
  (108 observations deleted due to missingness)
AIC: 511.57

Number of Fisher Scoring iterations: 4

> exp(lg.y$coefficients)
    (Intercept)  plingu02german plingu02italian        sexwoman 
      0.1886620       0.6226411       1.0800487       2.3214363 
> lg.y.coeff <- as.data.frame(summary(lg.y)$coefficients)
> lg.y.coeff <- cbind(lg.y.coeff, `OR`=exp(lg.y.coeff[,"Estimate"]))
> lg.y.coeff
                   Estimate Std. Error    z value     Pr(>|z|)        OR
(Intercept)     -1.66779806  0.2519296 -6.6200949 3.589684e-11 0.1886620
plingu02german  -0.47378508  0.2432392 -1.9478157 5.143703e-02 0.6226411
plingu02italian  0.07700611  0.4871571  0.1580724 8.743997e-01 1.0800487
sexwoman         0.84218610  0.2408458  3.4967854 4.709004e-04 2.3214363
> 
