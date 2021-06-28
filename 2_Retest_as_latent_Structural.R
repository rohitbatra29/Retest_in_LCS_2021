# Specification 2: Retest Effect as a Latent Covariate at the Structural Level 
# R code for fitting using dynr package

# Batra, R., Bunge, S. A., & Ferrer, E. (2021, in press). Modeling Retest Effects in 
# Developmental Processes Using Latent Change Score Models. 
# Structural Equation Modeling

# Loading Dynr

library(dynr)
# Please refer to this website for more info on setting up dynr and get cooking!

# Data Setup --------------------------------------------------------------

# The data is available upon request. This code can be easily edited for any other data. 
# Some of the important highlights for the data used here:
# 1) We use one measure for this specification: FR 
# 2) We model changes across age bins, which here are 15 age bins: 6-20
# 3) We need a data frame for coding retest coefficients, Gamma = (0, 1, 1) for up to three observations per individual
# 4) For dynr, the data needs to be in a long format, that is multiple rows per individual

# Suppose you have a data frame "fr_data" which contains the following columns:
# 1) id: id of individuals (15 rows per individual here)
# 2) FR: the score of fluid reasoning measure for each individual for each age bin
# 3) age_bin: 
# 3) gamma: the coefficient of gamma which takes values (0, 1, 1) for the 1st, 2nd and 3rd measurement occasion for an individual

# Below can be seen how the dataset looks for id 1. This individual had observations at 12th, 14th and 16th age bins so their retest coefficient is specified accordingly.

# id     FR   age_bin   gamma 
# 1      NA     6       0
# 1      NA	    7	      0
# 1	     NA	    8	      0
# 1	     NA	    9	      0
# 1	     NA	   10	      0
# 1	     NA	   11	      0
# 1	  -0.219	 12	      0
# 1	     NA	   13	      0
# 1	   0.900	 14	      1
# 1	     NA	   15	      0
# 1	   0.967	 16	      1
# 1	     NA	   17	      0
# 1	     NA	   18	      0
# 1	     NA	   19	      0
# 1	     NA	   20	      0
# 2	     NA	    6	      0
# 2	     NA	    7	      0
# :       :     :       :
# :       :     :       :

##########################################################################
# Retest with Age Bins Model (DT) ~ LV -----------------------------------
##########################################################################

# Data Prep for Dynr

data_ret_mod <- dynr.data(fr_data, id = "id", time = "age_bin", observed = "FR", covariates = "gamma")

# Measurement Model 

meas_bins_ret <- prep.measurement(
  values.load = matrix(c(1,0,0), ncol = 3), 
  params.load = matrix(rep("fixed", 3), ncol = 3),    
  state.names = c("Level", "Slope", "Practice"),       
  obs.names = c("FR")        
)

# Initial Conditions

initial_bins_ret <- prep.initial(
  values.inistate = c(-1, .5, .1),
  params.inistate = c('mu_level', 'mu_slope', 'mu_practice'),
  values.inicov = matrix(c(.2, .01, 0,
                           .01, .02, 0,
                           0, 0, .01), byrow = T, ncol = 3),
  params.inicov = matrix(c('v_level', 'cov_ls', 'fixed',
                           'cov_ls', 'v_slope', 'fixed',
                           'fixed', 'fixed', 'v_practice'), byrow = T, ncol = 3)
)

# RESIDUAL COVARIANCE MATRICES for Measurement and Dynamic Process model

mdcov_bins_ret <- prep.noise( 
  values.latent = diag(rep(0,3)),
  params.latent = diag(rep('fixed', 3)),
  values.observed = matrix(0.5),
  params.observed = diag(c("Error_Var"),1)
)

# Formula for Dynamic Model

form_ret_bins_dt <- list(
  list(Level ~ (1+beta)*Level + Slope + gamma*Practice,
       Slope ~ Slope,
       Practice ~ Practice)
)

dynm_ret_dt <- prep.formulaDynamics(formula = form_ret_bins_dt, startval = c(beta = -0.5), isContinuousTime = FALSE)

# Cooking the Model

mod_bins_ret_dt <- dynr.model(dynamics = dynm_ret_dt, measurement = meas_bins_ret, 
                              noise = mdcov_bins_ret, initial = initial_bins_ret, data = data_ret_mod, outfile = "FR_BINS_LDS.c")

LDS_Ret_DT <- dynr.cook(mod_bins_ret_dt, debug_flag = TRUE)

summary(LDS_Ret_DT) 

plotFormula(mod_bins_ret_dt, ParameterAs = mod_bins_ret_dt$'param.names')



