exam2
================
Meirou Guan
11/23/2020

1.(15 points) Please answer the following; you might find it useful to
make a sketch.

a.For a Normal Distribution that has mean 1 and standard deviation 6.5,
what is the area to the left of 1.65? anserer a:(0.53983)

2.  For a Normal Distribution that has mean 8 and standard deviation
    2.7, what is the area in both tails farther from the mean than
    13.67? anserer b: 0.017864\*2=0.035728

<!-- end list -->

    c.For a Normal Distribution that has mean -11 and standard deviation 4, what is the area in both tails farther from the mean than -5.4?
    anserer c:0.080757*2=0.161514
    
    d. For a Normal Distribution that has mean 14 and standard deviation 7.4 what two values leave probability 0.158 in both tails?
    
    
    e.A regression coefficient is estimated to be equal to 6.56 with standard error 4.1; there are 24 degrees of freedom. What is the p-value (from the t-statistic) against the null hypothesis of zero?
    answere: T-statistic= 1.6    The p-value=0.122681  (alpha=0.05)
    
    f.A regression coefficient is estimated to be equal to -0.24 with standard error 0.4; there are 4 degrees of freedom. What is the p-value (from the t-statistic) against the null hypothesis of zero?
    answer f: T-statistic= -0.6  The p-value=0.580841 (alpha=0.05)

2.(10 points) As we consider, “did everything change after March 2020?”
look at crude oil prices. The average daily return of crude oil was
0.000145 with standard deviation of 0.0213 in 289 days before March 1.
Average daily return after that date was -0.0210 with standard deviation
of 0.271 in 174 days after. Is there a statistically significant
difference in the mean? Calculate t-stat and p-value for the test
against no difference in daily returns.

Answer:

``` r
0.000145/0.0213
```

    ## [1] 0.006807512

Before March 1: T-statistic= 0.006807512

3.(10 points) In good news, there was information about vaccine trials.
Consider (these are not quite the actual data but a simplified version)
looking at 2 groups, each with 10,000 people. In the control group who
did not get the vaccine there were 90 infections. In the test group that
did get the vaccine there were 15 infections. Calculate the t-stat and
p-value for the test against no difference in infection rates between
groups.

The next questions will use the PUMS ACS 2017 NY data that we’ve been
using so often in class. We’ll consider people’s decisions about usual
hours worked, given by the variable UHRSWORK. There are 3 broad
categories: people who are not in the labor force, those working part
time (UHRSWORK \> 0 and \< 35) and those working full time ( \>= 35).

4.(10 points) Create a subgroup of the sample, that makes sense as we
focus on the decision of whether to work full time or part time. Explain
your rationale

``` r
load("~/Desktop/.RData")
attach(dat_NYC)
```

    ## The following object is masked _by_ .GlobalEnv:
    ## 
    ##     OWNCOST

``` r
use_varb <- (AGE >= 25) & (AGE <= 55) & (FAMSIZE >=5) & (Asian==1) & (educ_college==1)
dat_use <- subset(acs2017_ny,use_varb)
```

In this subset, we want to analyze whether an Asian person between 25 to
55 who has at least 4 family members, who has a college education level
would like to work as full time or part-time. Intuitively, a male has
more family members tend to be more likely to work as a full-time;
however, female on the opposite. People who has higer education level
also tend to work longer hours.

5.(10 points) Within this subgroup, test if there is a difference
between men and women. Calculate a t-stat and p-value for the test of no
difference between the groups. Provide simple statistics for a few other
relevanant categories.

``` r
model_sex <- glm(UHRSWORK ~  SEX,data=dat_use )
summary(model_sex)
```

    ## 
    ## Call:
    ## glm(formula = UHRSWORK ~ SEX, data = dat_use)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -24.102  -17.209   -5.209   18.791   81.791  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  24.1023     0.7953  30.307  < 2e-16 ***
    ## SEXFemale    -6.8930     1.0836  -6.361 2.64e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 445.2419)
    ## 
    ##     Null deviance: 696567  on 1525  degrees of freedom
    ## Residual deviance: 678549  on 1524  degrees of freedom
    ## AIC: 13641
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
anova(model_sex)
```

    ## Analysis of Deviance Table
    ## 
    ## Model: gaussian, link: identity
    ## 
    ## Response: UHRSWORK
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##      Df Deviance Resid. Df Resid. Dev
    ## NULL                  1525     696567
    ## SEX   1    18018      1524     678549

6.(20 points) Estimate a simple OLS model for hours worked, within your
subsample.

``` r
model2_UHRSWORK <- lm (UHRSWORK ~ AGE+I(AGE^2) +female  + OWNCOST+ RENT+MORTGAGE+FOODSTMP+ROOMS,data = dat_use)
summary(model2_UHRSWORK )
```

    ## 
    ## Call:
    ## lm(formula = UHRSWORK ~ AGE + I(AGE^2) + female + OWNCOST + RENT + 
    ##     MORTGAGE + FOODSTMP + ROOMS, data = dat_use)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -39.941 -12.472   2.486  11.576  71.888 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -8.936e+00  2.799e+00  -3.193  0.00144 ** 
    ## AGE          1.943e+00  6.727e-02  28.888  < 2e-16 ***
    ## I(AGE^2)    -2.128e-02  7.620e-04 -27.930  < 2e-16 ***
    ## female      -4.838e+00  8.517e-01  -5.681 1.60e-08 ***
    ## OWNCOST      1.979e-05  2.044e-05   0.968  0.33308    
    ## RENT         4.023e-03  9.051e-04   4.445 9.45e-06 ***
    ## MORTGAGE     1.696e+00  5.775e-01   2.937  0.00336 ** 
    ## FOODSTMP    -6.891e+00  1.229e+00  -5.606 2.46e-08 ***
    ## ROOMS        4.462e-01  1.818e-01   2.454  0.01423 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.5 on 1517 degrees of freedom
    ## Multiple R-squared:  0.4069, Adjusted R-squared:  0.4038 
    ## F-statistic: 130.1 on 8 and 1517 DF,  p-value: < 2.2e-16

a.Explain what variables you choose to use as predictors. Do they seem
exogenous? Consider whether polynomials in Age are important or
interactions with dummy variables.

Here, variables AGE, SEX, OWNCOST, RENT, FOODSTAMP and ROOM are
considered. Working hours varies from different age range so it is an
important factor that affects working hours. We think working hours is
closely related with owner cost such as whether a person rent a house or
whether he has a mortgage. Social benefit like foodstamp also affects
working hours. In the table above, the term AGE^2 is significant. It
helps the model to predict in which age range people work the most.

b.Do your estimates seem plausible? Are the estimates each statistically
significant?

At the significant level of 0.05, most of the variables selected are
significant, except for OWNCOST. Here, we think owncost is extensivly
related to many other factors which makes it indirect to explain working
hours.

c.Construct a joint test of whether a reasonable set of coefficients
(such as age polynomials, or education dummies) are all zero.

``` r
coef(model2_UHRSWORK)
```

    ##   (Intercept)           AGE      I(AGE^2)        female       OWNCOST 
    ## -8.9364675348  1.9432948766 -0.0212833212 -4.8380586393  0.0000197862 
    ##          RENT      MORTGAGE      FOODSTMP         ROOMS 
    ##  0.0040226932  1.6964641456 -6.8907340256  0.4461988084

``` 

```

d.What are the predicted probabilities for a few particular groups?

e.How many Type I and Type II errors are made by the model?

7.(20 points) Estimate a simple logit model, for the outcome variable
Work\_Fulltime \<- (UHRSWORK \>= 35), within your subsample.

    a.Explain what variables you choose to use as predictors. Do they seem exogenous? Consider whether polynomials in Age are important or interactions with dummy variables.

``` 
```

``` r
model4_WF <- glm ( UHRSWORK >= 35 ~ AGE+I(AGE^2) +female  + OWNCOST+ RENT+MORTGAGE+FOODSTMP+ROOMS,data = dat_use)
summary(model4_WF)
```

    ## 
    ## Call:
    ## glm(formula = UHRSWORK >= 35 ~ AGE + I(AGE^2) + female + OWNCOST + 
    ##     RENT + MORTGAGE + FOODSTMP + ROOMS, data = dat_use)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.84204  -0.36895   0.05035   0.35247   0.83383  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.636e-01  6.771e-02  -3.892 0.000104 ***
    ## AGE          4.125e-02  1.627e-03  25.349  < 2e-16 ***
    ## I(AGE^2)    -4.476e-04  1.843e-05 -24.280  < 2e-16 ***
    ## female      -1.056e-01  2.060e-02  -5.124 3.37e-07 ***
    ## OWNCOST      1.905e-07  4.943e-07   0.385 0.699995    
    ## RENT         1.042e-04  2.189e-05   4.760 2.12e-06 ***
    ## MORTGAGE     4.731e-02  1.397e-02   3.386 0.000727 ***
    ## FOODSTMP    -1.080e-01  2.973e-02  -3.631 0.000291 ***
    ## ROOMS        6.634e-03  4.398e-03   1.508 0.131666    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1593516)
    ## 
    ##     Null deviance: 369.20  on 1525  degrees of freedom
    ## Residual deviance: 241.74  on 1517  degrees of freedom
    ## AIC: 1538.9
    ## 
    ## Number of Fisher Scoring iterations: 2

b.Do your estimates seem plausible? Are the estimates each statistically
significant? In the logit model, the OWNCOST and ROOMS are
insignificant. All ohters factors show a credible relation with Working
as full-time.

    c.Construct a joint test of whether a reasonable set of coefficients (such as age polynomials, or education dummies) are all zero.

``` r
coef(model4_WF)
```

    ##   (Intercept)           AGE      I(AGE^2)        female       OWNCOST 
    ## -2.635528e-01  4.124981e-02 -4.475554e-04 -1.055729e-01  1.905149e-07 
    ##          RENT      MORTGAGE      FOODSTMP         ROOMS 
    ##  1.042134e-04  4.730635e-02 -1.079774e-01  6.633512e-03

d.What are the predicted probabilities for a few particular groups?
Estimating the probability a 35year-old male person who has foodstamp
f(female=0,age=35, foodstamp=1)=-2.636e-01+
4.125e-02\*(35)-4.476e-04(35^2) -1.080e-01

use 1/1+e^(f(x))=

e.How many Type I and Type II errors are made by the model?

``` r
 model4_WF<- glm( UHRSWORK >= 35 ~ AGE+I(AGE^2) +female  + OWNCOST+ RENT+MORTGAGE+FOODSTMP+ROOMS,data = dat_use)
suppressWarnings(pred_vals_lpm <- predict(model4_WF, dat_use))
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
table1 <- table(pred = pred_model_lpm1,true = dat_use$UHRSWORK)
summary(table1)
```

    ## Number of cases in table: 1526 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = 617.5, df = 54, p-value = 1.204e-96
    ##  Chi-squared approximation may be incorrect

``` r
print(table1)
```

    ##        true
    ## pred      0   2   3   4   6   8   9  10  11  12  13  15  16  17  18  20  23  24
    ##   FALSE 592   2   2   1   2   4   1  10   0   3   0   9   2   1   2  24   1   5
    ##   TRUE  126   0   1   1   1   3   0   1   1   2   1   6   6   0   0  14   0   5
    ##        true
    ## pred     25  27  28  30  32  34  35  36  37  38  39  40  41  42  43  44  45  46
    ##   FALSE  11   0   1  11   5   0  15   5   3   3   1  58   1   1   0   0   6   1
    ##   TRUE   11   3   3  20   5   1  33   8  13   8   2 271   0   1   3   3  36   2
    ##        true
    ## pred     47  48  49  50  51  52  55  56  58  60  65  68  70  72  75  80  84  90
    ##   FALSE   0   3   0   6   1   0   2   0   0   3   0   0   0   0   0   0   1   0
    ##   TRUE    1   8   1  59   0   2   9   2   1  36   3   1   5   1   2   2   0   1
    ##        true
    ## pred     99
    ##   FALSE   0
    ##   TRUE    2

8.(25 points) Estimate one or more additional models with other methods
(not OLS or logit) to predict hours or if a person works full time.
Explain as in previous. Compare the different models and make judgements
about strengths and weaknesses of each.

\``>set.seed(54321) model1_spikeslab <- spikeslab UHRSWORK >= 35 ~
AGE+I(AGE^2) +female + OWNCOST+ RENT+MORTGAGE+FOODSTMP+ROOMS,data =
dat_use() >summary(model1_spikeslab) >print(model1_spikeslab)
>plot(model1_spikeslab)`{r}

``` 
```
