# Specification 1: Retest Effect as an Observed Predictor at the Structural Level
# R code for fitting using lavaan package


# Batra, R., Bunge, S. A., & Ferrer, E. (2021, in press). Modeling Retest Effects in 
# Developmental Processes Using Latent Change Score Models. 
# Structural Equation Modeling

library(lavaan)


# Data Setup --------------------------------------------------------------

# The data is available upon request. This code can be easily edited for any other data. 
# Some of the important highlights for the data used here:
# 1) We use one measure for this specification: FR 
# 2) We model changes across age bins, which here are 15 age bins: 6-20
# 3) We need a data frame for coding retest predictor as (0, 1, 1) for up to three observations per individual

# Suppose you have a data frame "fr_data" which contains the following columns:
# 1) id: id of individuals
# 2) times: how many times an individual came in for a study (1, 2 or 3 for our example here)
# 3) age bins: 15 columns representing the different age bins

# Now, let's recode the data into a retest predictor R[t] where:
# an individual with only one observation, gets a R[t] = (0,0,0)
# an individual with only two observations, gets a R[t] = (0,1,0)
# an individual with three observations, gets a R[t] = (0, 1, 1) 
# This is because we assume a one-step increase in practice and
# this can be easily constrained to be linear or any other function 
# based on the researcher's hypothesis.

newd <- as.matrix(fr_data)   #matrix form of the data
times <- fr_data$times

retest_cov <- matrix(rep(0, 201*15), nrow = 201, ncol = 15) # since we have 201 observations

for (i in c(1:201)) {
  col_index <- which(!is.na(newd[i,c(3:17)]))
  if (times[i] == 3) {
    retest_cov[i, col_index] = c(0,1,1)
  } else if (times[i] == 2){
    retest_cov[i, col_index] = c(0,1)
  } else {
    retest_cov[i, col_index] = c(0)
  }
}

colnames(retest_cov) <- c("R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", "R18", "R19", "R20")

new_d <- cbind(new_d, retest_cov)


# 1LCS without any retest effects -----------------------------------------

# To start, we fit a First-Order LCS (1LCS) without any retest effects

lds_mod1 <- '
# Defining latent variables on observed variables

lv6 =~ 1*FR6
lv7 =~ 1*FR7
lv8 =~ 1*FR8
lv9 =~ 1*FR9
lv10 =~ 1*FR10
lv11 =~ 1*FR11
lv12 =~ 1*FR12
lv13 =~ 1*FR13
lv14 =~ 1*FR14
lv15 =~ 1*FR15
lv16 =~ 1*FR16
lv17 =~ 1*FR17
lv18 =~ 1*FR18
lv19 =~ 1*FR19
lv20 =~ 1*FR20

# Autoregression of Latent variables

lv7 ~ 1*lv6
lv8 ~ 1*lv7
lv9 ~ 1*lv8
lv10 ~ 1*lv9
lv11 ~ 1*lv10
lv12 ~ 1*lv11
lv13 ~ 1*lv12
lv14 ~ 1*lv13
lv15 ~ 1*lv14
lv16 ~ 1*lv15
lv17 ~ 1*lv16
lv18 ~ 1*lv17
lv19 ~ 1*lv18
lv20 ~ 1*lv19

# Defining latent change on latent variables

ldv7 =~ 1*lv7
ldv8 =~ 1*lv8
ldv9 =~ 1*lv9
ldv10 =~ 1*lv10
ldv11 =~ 1*lv11
ldv12 =~ 1*lv12
ldv13 =~ 1*lv13
ldv14 =~ 1*lv14
ldv15 =~ 1*lv15
ldv16 =~ 1*lv16
ldv17 =~ 1*lv17
ldv18 =~ 1*lv18
ldv19 =~ 1*lv19
ldv20 =~ 1*lv20

# Autoproportions (Feedback) of latent change

ldv7 ~ b*lv6
ldv8 ~ b*lv7
ldv9 ~ b*lv8
ldv10 ~ b*lv9
ldv11 ~ b*lv10
ldv12 ~ b*lv11
ldv13 ~ b*lv12
ldv14 ~ b*lv13
ldv15 ~ b*lv14
ldv16 ~ b*lv15
ldv17 ~ b*lv16
ldv18 ~ b*lv17
ldv19 ~ b*lv18
ldv20 ~ b*lv19

# Defining intercept and slope

y0 =~ 1*lv6
ys =~ 1*ldv7
ys =~ 1*ldv8
ys =~ 1*ldv9
ys =~ 1*ldv10
ys =~ 1*ldv11
ys =~ 1*ldv12
ys =~ 1*ldv13
ys =~ 1*ldv14
ys =~ 1*ldv15
ys =~ 1*ldv16
ys =~ 1*ldv17
ys =~ 1*ldv18
ys =~ 1*ldv19
ys =~ 1*ldv20


# Means of intercept and slope

y0 ~ 1
ys ~ 1

# Means of latent variables

lv6 ~ 0
lv7 ~ 0
lv8 ~ 0
lv9 ~ 0
lv10 ~ 0
lv11 ~ 0
lv12 ~ 0
lv13 ~ 0
lv14 ~ 0
lv15 ~ 0
lv16 ~ 0
lv17 ~ 0
lv18 ~ 0
lv19 ~ 0
lv20 ~ 0

# Means of latent change variables

ldv7 ~ 0
ldv8 ~ 0
ldv9 ~ 0
ldv10 ~ 0
ldv11 ~ 0
ldv12 ~ 0
ldv13 ~ 0
ldv14 ~ 0
ldv15 ~ 0
ldv16 ~ 0
ldv17 ~ 0
ldv18 ~ 0
ldv19 ~ 0
ldv20 ~ 0

# Means of Manifest Variables set to 0

FR6 ~ 0
FR7 ~ 0
FR8 ~ 0
FR9 ~ 0
FR10 ~ 0
FR11 ~ 0
FR12 ~ 0
FR13 ~ 0
FR14 ~ 0
FR15 ~ 0
FR16 ~ 0
FR17 ~ 0
FR18 ~ 0
FR19 ~ 0
FR20 ~ 0

# Var/Covariance of intercept and slope

y0 ~~ ys
y0 ~~ y0
ys ~~ ys

# Variance of Latent change set to 0 (residauls)

ldv7 ~~ 0*ldv7
ldv8 ~~ 0*ldv8
ldv9 ~~ 0*ldv9
ldv10 ~~ 0*ldv10
ldv11 ~~ 0*ldv11
ldv12 ~~ 0*ldv12
ldv13 ~~ 0*ldv13
ldv14 ~~ 0*ldv14
ldv15 ~~ 0*ldv15
ldv16 ~~ 0*ldv16
ldv17 ~~ 0*ldv17
ldv18 ~~ 0*ldv18
ldv19 ~~ 0*ldv19
ldv20 ~~ 0*ldv20

# Variance of Latent variables set to 0 (residuals)

lv6 ~~ 0*lv6
lv7 ~~ 0*lv7
lv8 ~~ 0*lv8
lv9 ~~ 0*lv9
lv10 ~~ 0*lv10
lv11 ~~ 0*lv11
lv12 ~~ 0*lv12
lv13 ~~ 0*lv13
lv14 ~~ 0*lv14
lv15 ~~ 0*lv15
lv16 ~~ 0*lv16
lv17 ~~ 0*lv17
lv18 ~~ 0*lv18
lv19 ~~ 0*lv19
lv20 ~~ 0*lv20

# Residual Variances of manifest variables to be equal 

FR6 ~~ error_var*FR6
FR7 ~~ error_var*FR7
FR8 ~~ error_var*FR8
FR9 ~~ error_var*FR9
FR10 ~~ error_var*FR10
FR11 ~~ error_var*FR11
FR12 ~~ error_var*FR12
FR13 ~~ error_var*FR13
FR14 ~~ error_var*FR14
FR15 ~~ error_var*FR15
FR16 ~~ error_var*FR16
FR17 ~~ error_var*FR17
FR18 ~~ error_var*FR18
FR19 ~~ error_var*FR19
FR20 ~~ error_var*FR20'

fit_lds1 <- sem(model = lds_mod1, data = new_d, missing = "fiml")

summary(fit_lds1, standardized = TRUE, fit.measures = TRUE)


# 1LCS with lv ~ retest effect (one gamma) -------------------------------------

# We fit a 1LCS with an observed retest covariate on the latent true score variables
# and with only one gamma coefficient across all age bins. 

lds_mod2 <- '
# Defining latent variables on observed variables

lv6 =~ 1*FR6
lv7 =~ 1*FR7
lv8 =~ 1*FR8
lv9 =~ 1*FR9
lv10 =~ 1*FR10
lv11 =~ 1*FR11
lv12 =~ 1*FR12
lv13 =~ 1*FR13
lv14 =~ 1*FR14
lv15 =~ 1*FR15
lv16 =~ 1*FR16
lv17 =~ 1*FR17
lv18 =~ 1*FR18
lv19 =~ 1*FR19
lv20 =~ 1*FR20

# Autoregression of Latent variables

lv7 ~ 1*lv6 + gamma*R7
lv8 ~ 1*lv7 + gamma*R8
lv9 ~ 1*lv8 + gamma*R9
lv10 ~ 1*lv9 + gamma*R10
lv11 ~ 1*lv10 + gamma*R11
lv12 ~ 1*lv11 + gamma*R12
lv13 ~ 1*lv12 + gamma*R13
lv14 ~ 1*lv13 + gamma*R14
lv15 ~ 1*lv14 + gamma*R15
lv16 ~ 1*lv15 + gamma*R16
lv17 ~ 1*lv16 + gamma*R17
lv18 ~ 1*lv17 + gamma*R18
lv19 ~ 1*lv18 + gamma*R19
lv20 ~ 1*lv19 + gamma*R20


# Defining latent change on latent variables

ldv7 =~ 1*lv7
ldv8 =~ 1*lv8
ldv9 =~ 1*lv9
ldv10 =~ 1*lv10
ldv11 =~ 1*lv11
ldv12 =~ 1*lv12
ldv13 =~ 1*lv13
ldv14 =~ 1*lv14
ldv15 =~ 1*lv15
ldv16 =~ 1*lv16
ldv17 =~ 1*lv17
ldv18 =~ 1*lv18
ldv19 =~ 1*lv19
ldv20 =~ 1*lv20

# Autoproportions (Feedback) of latent change

ldv7 ~ b*lv6
ldv8 ~ b*lv7
ldv9 ~ b*lv8
ldv10 ~ b*lv9
ldv11 ~ b*lv10
ldv12 ~ b*lv11
ldv13 ~ b*lv12
ldv14 ~ b*lv13
ldv15 ~ b*lv14
ldv16 ~ b*lv15
ldv17 ~ b*lv16
ldv18 ~ b*lv17
ldv19 ~ b*lv18
ldv20 ~ b*lv19

# Defining intercept and slope

y0 =~ 1*lv6
ys =~ 1*ldv7
ys =~ 1*ldv8
ys =~ 1*ldv9
ys =~ 1*ldv10
ys =~ 1*ldv11
ys =~ 1*ldv12
ys =~ 1*ldv13
ys =~ 1*ldv14
ys =~ 1*ldv15
ys =~ 1*ldv16
ys =~ 1*ldv17
ys =~ 1*ldv18
ys =~ 1*ldv19
ys =~ 1*ldv20

# Means of intercept and slope

y0 ~ 1
ys ~ 1

# Means of latent variables

lv6 ~ 0
lv7 ~ 0
lv8 ~ 0
lv9 ~ 0
lv10 ~ 0
lv11 ~ 0
lv12 ~ 0
lv13 ~ 0
lv14 ~ 0
lv15 ~ 0
lv16 ~ 0
lv17 ~ 0
lv18 ~ 0
lv19 ~ 0
lv20 ~ 0

# Means of latent change variables

ldv7 ~ 0
ldv8 ~ 0
ldv9 ~ 0
ldv10 ~ 0
ldv11 ~ 0
ldv12 ~ 0
ldv13 ~ 0
ldv14 ~ 0
ldv15 ~ 0
ldv16 ~ 0
ldv17 ~ 0
ldv18 ~ 0
ldv19 ~ 0
ldv20 ~ 0

# Means of Manifest Variables set to 0

FR6 ~ 0
FR7 ~ 0
FR8 ~ 0
FR9 ~ 0
FR10 ~ 0
FR11 ~ 0
FR12 ~ 0
FR13 ~ 0
FR14 ~ 0
FR15 ~ 0
FR16 ~ 0
FR17 ~ 0
FR18 ~ 0
FR19 ~ 0
FR20 ~ 0

# Var/Covariance of intercept and slope

y0 ~~ ys
y0 ~~ y0
ys ~~ ys

# Variance of Latent change set to 0 (residauls)

ldv7 ~~ 0*ldv7
ldv8 ~~ 0*ldv8
ldv9 ~~ 0*ldv9
ldv10 ~~ 0*ldv10
ldv11 ~~ 0*ldv11
ldv12 ~~ 0*ldv12
ldv13 ~~ 0*ldv13
ldv14 ~~ 0*ldv14
ldv15 ~~ 0*ldv15
ldv16 ~~ 0*ldv16
ldv17 ~~ 0*ldv17
ldv18 ~~ 0*ldv18
ldv19 ~~ 0*ldv19
ldv20 ~~ 0*ldv20

# Variance of Latent variables set to 0 (residuals)

lv6 ~~ 0*lv6
lv7 ~~ 0*lv7
lv8 ~~ 0*lv8
lv9 ~~ 0*lv9
lv10 ~~ 0*lv10
lv11 ~~ 0*lv11
lv12 ~~ 0*lv12
lv13 ~~ 0*lv13
lv14 ~~ 0*lv14
lv15 ~~ 0*lv15
lv16 ~~ 0*lv16
lv17 ~~ 0*lv17
lv18 ~~ 0*lv18
lv19 ~~ 0*lv19
lv20 ~~ 0*lv20

# Residual Variances of manifest variables to be equal 

FR6 ~~ c*FR6
FR7 ~~ c*FR7
FR8 ~~ c*FR8
FR9 ~~ c*FR9
FR10 ~~ c*FR10
FR11 ~~ c*FR11
FR12 ~~ c*FR12
FR13 ~~ c*FR13
FR14 ~~ c*FR14
FR15 ~~ c*FR15
FR16 ~~ c*FR16
FR17 ~~ c*FR17
FR18 ~~ c*FR18
FR19 ~~ c*FR19
FR20 ~~ c*FR20'

fit_lds2 <- sem(model = lds_mod2, data = new_d, missing = "fiml")

summary(fit_lds2, standardized = TRUE, fit.measures = TRUE)


# 1LCS with ldv ~ retest effect -------------------------------------------

# This model is the same as above but now retest is a covariate of the changes
# with only one gamma coefficient. This model will give exactly the same estimates
# as the previous one. It depends on the researcher if they want to use retest
# as a predictor of the true scores or the changes in the true scores. 

lds_mod3 <- '
# Defining latent variables on observed variables

lv6 =~ 1*FR6
lv7 =~ 1*FR7
lv8 =~ 1*FR8
lv9 =~ 1*FR9
lv10 =~ 1*FR10
lv11 =~ 1*FR11
lv12 =~ 1*FR12
lv13 =~ 1*FR13
lv14 =~ 1*FR14
lv15 =~ 1*FR15
lv16 =~ 1*FR16
lv17 =~ 1*FR17
lv18 =~ 1*FR18
lv19 =~ 1*FR19
lv20 =~ 1*FR20

# Autoregression of Latent variables

lv7 ~ 1*lv6
lv8 ~ 1*lv7
lv9 ~ 1*lv8
lv10 ~ 1*lv9
lv11 ~ 1*lv10
lv12 ~ 1*lv11
lv13 ~ 1*lv12
lv14 ~ 1*lv13
lv15 ~ 1*lv14
lv16 ~ 1*lv15
lv17 ~ 1*lv16
lv18 ~ 1*lv17
lv19 ~ 1*lv18
lv20 ~ 1*lv19

# Defining latent change on latent variables

ldv7 =~ 1*lv7
ldv8 =~ 1*lv8
ldv9 =~ 1*lv9
ldv10 =~ 1*lv10
ldv11 =~ 1*lv11
ldv12 =~ 1*lv12
ldv13 =~ 1*lv13
ldv14 =~ 1*lv14
ldv15 =~ 1*lv15
ldv16 =~ 1*lv16
ldv17 =~ 1*lv17
ldv18 =~ 1*lv18
ldv19 =~ 1*lv19
ldv20 =~ 1*lv20

# Autoproportions (Feedback) of latent change

ldv7 ~ b*lv6 + gamma*R7
ldv8 ~ b*lv7 + gamma*R8
ldv9 ~ b*lv8 + gamma*R9
ldv10 ~ b*lv9 + gamma*R10
ldv11 ~ b*lv10 + gamma*R11
ldv12 ~ b*lv11 + gamma*R12
ldv13 ~ b*lv12 + gamma*R13
ldv14 ~ b*lv13 + gamma*R14
ldv15 ~ b*lv14 + gamma*R15
ldv16 ~ b*lv15 + gamma*R16
ldv17 ~ b*lv16 + gamma*R17
ldv18 ~ b*lv17 + gamma*R18
ldv19 ~ b*lv18 + gamma*R19
ldv20 ~ b*lv19 + gamma*R20

# Defining intercept and slope

y0 =~ 1*lv6
ys =~ 1*ldv7
ys =~ 1*ldv8
ys =~ 1*ldv9
ys =~ 1*ldv10
ys =~ 1*ldv11
ys =~ 1*ldv12
ys =~ 1*ldv13
ys =~ 1*ldv14
ys =~ 1*ldv15
ys =~ 1*ldv16
ys =~ 1*ldv17
ys =~ 1*ldv18
ys =~ 1*ldv19
ys =~ 1*ldv20

# Means of intercept and slope

y0 ~ 1
ys ~ 1

# Means of latent variables

lv6 ~ 0
lv7 ~ 0
lv8 ~ 0
lv9 ~ 0
lv10 ~ 0
lv11 ~ 0
lv12 ~ 0
lv13 ~ 0
lv14 ~ 0
lv15 ~ 0
lv16 ~ 0
lv17 ~ 0
lv18 ~ 0
lv19 ~ 0
lv20 ~ 0

# Means of latent change variables

ldv7 ~ 0
ldv8 ~ 0
ldv9 ~ 0
ldv10 ~ 0
ldv11 ~ 0
ldv12 ~ 0
ldv13 ~ 0
ldv14 ~ 0
ldv15 ~ 0
ldv16 ~ 0
ldv17 ~ 0
ldv18 ~ 0
ldv19 ~ 0
ldv20 ~ 0

# Means of Manifest Variables set to 0

FR6 ~ 0
FR7 ~ 0
FR8 ~ 0
FR9 ~ 0
FR10 ~ 0
FR11 ~ 0
FR12 ~ 0
FR13 ~ 0
FR14 ~ 0
FR15 ~ 0
FR16 ~ 0
FR17 ~ 0
FR18 ~ 0
FR19 ~ 0
FR20 ~ 0

# Var/Covariance of intercept and slope

y0 ~~ ys
y0 ~~ y0
ys ~~ ys

# Variance of Latent change set to 0 (residauls)

ldv7 ~~ 0*ldv7
ldv8 ~~ 0*ldv8
ldv9 ~~ 0*ldv9
ldv10 ~~ 0*ldv10
ldv11 ~~ 0*ldv11
ldv12 ~~ 0*ldv12
ldv13 ~~ 0*ldv13
ldv14 ~~ 0*ldv14
ldv15 ~~ 0*ldv15
ldv16 ~~ 0*ldv16
ldv17 ~~ 0*ldv17
ldv18 ~~ 0*ldv18
ldv19 ~~ 0*ldv19
ldv20 ~~ 0*ldv20

# Variance of Latent variables set to 0 (residuals)

lv6 ~~ 0*lv6
lv7 ~~ 0*lv7
lv8 ~~ 0*lv8
lv9 ~~ 0*lv9
lv10 ~~ 0*lv10
lv11 ~~ 0*lv11
lv12 ~~ 0*lv12
lv13 ~~ 0*lv13
lv14 ~~ 0*lv14
lv15 ~~ 0*lv15
lv16 ~~ 0*lv16
lv17 ~~ 0*lv17
lv18 ~~ 0*lv18
lv19 ~~ 0*lv19
lv20 ~~ 0*lv20

# Residual Variances of manifest variables to be equal 

FR6 ~~ c*FR6
FR7 ~~ c*FR7
FR8 ~~ c*FR8
FR9 ~~ c*FR9
FR10 ~~ c*FR10
FR11 ~~ c*FR11
FR12 ~~ c*FR12
FR13 ~~ c*FR13
FR14 ~~ c*FR14
FR15 ~~ c*FR15
FR16 ~~ c*FR16
FR17 ~~ c*FR17
FR18 ~~ c*FR18
FR19 ~~ c*FR19
FR20 ~~ c*FR20'

fit_lds3 <- sem(model = lds_mod3, data = new_d, missing = "fiml")

summary(fit_lds3, standardized = TRUE, fit.measures = TRUE)

# 1LCS with retest effect (multiple gamma) --------------------------------

# We fit a 1LCS with an observed retest covariate but now with multiple gamma
# coefficients for three age groups: (6, 10), (11, 15), (16,20).

lds_mod4 <- '
# Defining latent variables on observed variables

lv6 =~ 1*FR6
lv7 =~ 1*FR7
lv8 =~ 1*FR8
lv9 =~ 1*FR9
lv10 =~ 1*FR10
lv11 =~ 1*FR11
lv12 =~ 1*FR12
lv13 =~ 1*FR13
lv14 =~ 1*FR14
lv15 =~ 1*FR15
lv16 =~ 1*FR16
lv17 =~ 1*FR17
lv18 =~ 1*FR18
lv19 =~ 1*FR19
lv20 =~ 1*FR20

# Autoregression of Latent variables

lv7 ~ 1*lv6 + gamma1*R7
lv8 ~ 1*lv7 + gamma1*R8
lv9 ~ 1*lv8 + gamma1*R9
lv10 ~ 1*lv9 + gamma1*R10
lv11 ~ 1*lv10 + gamma2*R11
lv12 ~ 1*lv11 + gamma2*R12
lv13 ~ 1*lv12 + gamma2*R13
lv14 ~ 1*lv13 + gamma2*R14
lv15 ~ 1*lv14 + gamma2*R15
lv16 ~ 1*lv15 + gamma3*R16
lv17 ~ 1*lv16 + gamma3*R17
lv18 ~ 1*lv17 + gamma3*R18
lv19 ~ 1*lv18 + gamma3*R19
lv20 ~ 1*lv19 + gamma3*R20


# Defining latent change on latent variables

ldv7 =~ 1*lv7
ldv8 =~ 1*lv8
ldv9 =~ 1*lv9
ldv10 =~ 1*lv10
ldv11 =~ 1*lv11
ldv12 =~ 1*lv12
ldv13 =~ 1*lv13
ldv14 =~ 1*lv14
ldv15 =~ 1*lv15
ldv16 =~ 1*lv16
ldv17 =~ 1*lv17
ldv18 =~ 1*lv18
ldv19 =~ 1*lv19
ldv20 =~ 1*lv20

# Autoproportions (Feedback) of latent change

ldv7 ~ b*lv6
ldv8 ~ b*lv7
ldv9 ~ b*lv8
ldv10 ~ b*lv9
ldv11 ~ b*lv10
ldv12 ~ b*lv11
ldv13 ~ b*lv12
ldv14 ~ b*lv13
ldv15 ~ b*lv14
ldv16 ~ b*lv15
ldv17 ~ b*lv16
ldv18 ~ b*lv17
ldv19 ~ b*lv18
ldv20 ~ b*lv19

# Defining intercept and slope

y0 =~ 1*lv6
ys =~ 1*ldv7
ys =~ 1*ldv8
ys =~ 1*ldv9
ys =~ 1*ldv10
ys =~ 1*ldv11
ys =~ 1*ldv12
ys =~ 1*ldv13
ys =~ 1*ldv14
ys =~ 1*ldv15
ys =~ 1*ldv16
ys =~ 1*ldv17
ys =~ 1*ldv18
ys =~ 1*ldv19
ys =~ 1*ldv20

# Means of intercept and slope

y0 ~ 1
ys ~ 1

# Means of latent variables

lv6 ~ 0
lv7 ~ 0
lv8 ~ 0
lv9 ~ 0
lv10 ~ 0
lv11 ~ 0
lv12 ~ 0
lv13 ~ 0
lv14 ~ 0
lv15 ~ 0
lv16 ~ 0
lv17 ~ 0
lv18 ~ 0
lv19 ~ 0
lv20 ~ 0

# Means of latent change variables

ldv7 ~ 0
ldv8 ~ 0
ldv9 ~ 0
ldv10 ~ 0
ldv11 ~ 0
ldv12 ~ 0
ldv13 ~ 0
ldv14 ~ 0
ldv15 ~ 0
ldv16 ~ 0
ldv17 ~ 0
ldv18 ~ 0
ldv19 ~ 0
ldv20 ~ 0

# Means of Manifest Variables set to 0

FR6 ~ 0
FR7 ~ 0
FR8 ~ 0
FR9 ~ 0
FR10 ~ 0
FR11 ~ 0
FR12 ~ 0
FR13 ~ 0
FR14 ~ 0
FR15 ~ 0
FR16 ~ 0
FR17 ~ 0
FR18 ~ 0
FR19 ~ 0
FR20 ~ 0

# Var/Covariance of intercept and slope

y0 ~~ ys
y0 ~~ y0
ys ~~ ys

# Variance of Latent change set to 0 (residauls)

ldv7 ~~ 0*ldv7
ldv8 ~~ 0*ldv8
ldv9 ~~ 0*ldv9
ldv10 ~~ 0*ldv10
ldv11 ~~ 0*ldv11
ldv12 ~~ 0*ldv12
ldv13 ~~ 0*ldv13
ldv14 ~~ 0*ldv14
ldv15 ~~ 0*ldv15
ldv16 ~~ 0*ldv16
ldv17 ~~ 0*ldv17
ldv18 ~~ 0*ldv18
ldv19 ~~ 0*ldv19
ldv20 ~~ 0*ldv20

# Variance of Latent variables set to 0 (residuals)

lv6 ~~ 0*lv6
lv7 ~~ 0*lv7
lv8 ~~ 0*lv8
lv9 ~~ 0*lv9
lv10 ~~ 0*lv10
lv11 ~~ 0*lv11
lv12 ~~ 0*lv12
lv13 ~~ 0*lv13
lv14 ~~ 0*lv14
lv15 ~~ 0*lv15
lv16 ~~ 0*lv16
lv17 ~~ 0*lv17
lv18 ~~ 0*lv18
lv19 ~~ 0*lv19
lv20 ~~ 0*lv20

# Residual Variances of manifest variables to be equal 

FR6 ~~ c*FR6
FR7 ~~ c*FR7
FR8 ~~ c*FR8
FR9 ~~ c*FR9
FR10 ~~ c*FR10
FR11 ~~ c*FR11
FR12 ~~ c*FR12
FR13 ~~ c*FR13
FR14 ~~ c*FR14
FR15 ~~ c*FR15
FR16 ~~ c*FR16
FR17 ~~ c*FR17
FR18 ~~ c*FR18
FR19 ~~ c*FR19
FR20 ~~ c*FR20'

fit_lds4 <- sem(model = lds_mod4, data = new_d, missing = "fiml")

summary(fit_lds4, standardized = TRUE, fit.measures = TRUE)
