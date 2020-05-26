#########################################
#                                       #
#   Example for multilevel imputation   #
#                                       # 
#########################################

# The present example uses the R package jomo.
# For details on jomo and a tutorial please see: 
# Quartagno, M., Grund, S., & Carpenter, J. (2019). 
# jomo: A Flexible Package for Two-level Joint Modelling Multiple Imputation. R Journal.


# Set random number generator
set.seed(1234)

# Load libraries
library(jomo) ; library(lme4) ; library(mitools) ; library(mitml)


##################################################
# Simple simulation of data with nested structure

# Sample size
N <- 100

# Variables in the model
soc.connection     <- round(abs(rnorm(N, 2, sd = 5)), digits = 2)    # degree of perceived social connection
time               <- rep(1:4, N)                                    # Assessment time (in years; year 1, year 2, year 3, year 4)
subjectno          <- as.factor(rep(1:N, each = 4))                  # Subject variable
depressivity       <- rnorm(N, 22, sd = 4.5)                         # Level 2 variable depressivity
intercept.variance <- rep(rnorm(N, 0, sd = 2.5), each = 4)           # Random intercept variance for each individual



# Simulating the outcome variable (mood) from assumed underlying model 
# considering the nested structure of the data as well as error variance 

# Increased mood is linked to a higher degree of perceived social connection (level 1 predictor)
# and less depressivity at baseline (level 2 predictor)
mood <- (20 + intercept.variance)  + 0.1*time + 0.4*soc.connection + (-0.7)*depressivity + rnorm(N, 0, sd = 3.5)

# Combine to data.frame
Data <- data.frame(mood, soc.connection, time, depressivity, subjectno)


# Create missing data that are Missing Completely At Random (MCAR):
# Randomly delete 20 observations in the outcome variable
Data[sample(nrow(Data), 20), c("mood")] <- NA

# Check
table(is.na(Data$time))


# Test model with complete data only
model1 <- lmer(mood ~ soc.connection + time + depressivity + (1 | subjectno), data = Data)
summary(model1)



############################################
# Imputation 

# Intercept variable for imputation model
Data$cons <- 1


# Outcome variable which will be imputed
Y              <- as.data.frame(Data$mood)

# Predictors for the imputation
X              <- as.data.frame(Data[, c("soc.connection", "time", "depressivity")])


# Run jomo 
imp <- jomo(Y = Y, X = X, clus = Data$subjectno, nburn = 3000, nbetween = 1000, nimp = 20)


# Analysing the imputed data
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
fit.imp  <- with(data = imp.list, lmer(mood ~ soc.connection + time + depressivity + (1 | subjectno), data = Data))

# Test estimates
testEstimates(fit.imp, var.comp = TRUE)

# Extract coefficients and variances
coefs    <- MIextract(fit.imp, fun = fixef)
vars     <- MIextract(fit.imp, fun = function(x) diag(vcov(x)))

# Pool results
results  <- MIcombine(coefs, vars)
summary(results)
