# 'Boundary problem' of SEM?
library(lavaan); library(semPlot)

          # Including other measures increases model fit:

TrueModel1 <- ' # Truemodel 1 is a vacuum scenario, where measure Z is analysed alone.
# Error/Disturbance
ErrorFactor1 =~ .2*Z1 + .2*Z2 + .2*Z3 + .2*Z4 + .2*Z5 + .2*Z6 + .2*Z7 + .2*Z8

# Error/Disturbance covariances
Z1 ~~ .35*Z2
Z3 ~~ .35*Z4
Z5 ~~ .35*Z6

# Factor of interest per se.
F3 =~ .7*Z1 + .7*Z2 + .7*Z3 + .7*Z4 + .7*Z5 + .7*Z6 + .7*Z7 + .7*Z8

# Factors are orthogonal.
F3 ~~ 0*ErrorFactor
'

FitModel1 <- " # Fitmodel 1 is the respective model to be fit, without the error factor.
F3 =~ Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8
"

# Plot scenario 1
par(mfrow = c(2,1))
semPaths(lavaanify(TrueModel1), what = "path")
semPaths(lavaanify(FitModel1), what = "path")
par(mfrow = c(1,1))

TrueModel2 <- ' # Truemodel 2 is a scenario where multiple measures are included to form a joint analysis. The added measures fit perfectly.
# Other measures, say PHQ-9, OASIS
F1 =~ .7*X1 + .7*X2 + .7*X3 + .7*X4
F2 =~ .7*Y1 + .7*Y2 + .7*Y3 + .7*Y4

# Error/Disturbance
ErrorFactor1 =~ .2*Z1 + .2*Z2 + .2*Z3 + .2*Z4 + .2*Z5 + .2*Z6 + .2*Z7 + .2*Z8

# Error/Disturbance covariances
Z1 ~~ .35*Z2
Z3 ~~ .35*Z4
Z5 ~~ .35*Z6

# Factor of interest per se.
F3 =~ .7*Z1 + .7*Z2 + .7*Z3 + .7*Z4 + .7*Z5 + .7*Z6 + .7*Z7 + .7*Z8

# Factors are orthogonal.
F3 ~~ 0*ErrorFactor + 0*F2 + 0*F1
F2 ~~ 0*ErrorFactor + 0*F1
F1 ~~ 0*ErrorFactor
'

FitModel2 <- " # Fitmodel 2 is the respective model to be fit, without the error factor.
# Other measures, say PHQ-9, OASIS, are recognized.
F1 =~ X1 + X2 + X3 + X4
F2 =~ Y1 + Y2 + Y3 + Y4

# Factor of interest per se.
F3 =~ Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7 + Z8

# Factors are assumed orthogonal, as they in truth are.
F3 ~~ 0*F2 + 0*F1
F2 ~~ 0*F1
"

# Plot scenario two
par(mfrow = c(2,1))
semPaths(lavaanify(TrueModel2), what = "path")
semPaths(lavaanify(FitModel2), what = "path")
par(mfrow = c(1,1))

# Result matrix.
Results <- array(dim = c(1000,2,3))
colnames(Results) <- c("Scenario1", "Scenario2")

sapply(1:1000, FUN = function(i) {
  # Simulate
  TrueData1 <- simulateData(TrueModel1, sample.nobs = 5000)
  TrueData2 <- simulateData(TrueModel2, sample.nobs = 5000)
  
  # Fit
  FitScenario1 <- sem(model = FitModel1, TrueData1)
  FitScenario2 <- sem(model = FitModel2, TrueData2)
  
  # Capture CFI
  Results[ i , 1 , 1 ] <<- fitMeasures( FitScenario1, "CFI" )
  Results[ i , 2 , 1 ] <<- fitMeasures( FitScenario2, "CFI" )
  # Capture RMSEA
  Results[ i , 1 , 2 ] <<- fitMeasures( FitScenario1, "RMSEA" )
  Results[ i , 2 , 2 ] <<- fitMeasures( FitScenario2, "RMSEA" )
  # Capture SRMR
  Results[ i , 1 , 3 ] <<- fitMeasures( FitScenario1, "SRMR" )
  Results[ i , 2 , 3 ] <<- fitMeasures( FitScenario2, "SRMR" )
  # -> Repeat.
})

# Plot results
plot(density(Results[ , 1, 1]), xlim = c(0,1), xticks = c(seq(0,1,.1)))
lines(density(Results[ , 2, 1]), lty = 2) # Scenario 2, where additional measures are analysed jointly has consistently higher CFI.
plot(density(Results[ , 1, 2]), xlim = c(0,1))
lines(density(Results[ , 2, 2]), lty = 2) # As well as RMSEA, and
plot(density(Results[ , 1, 3]), xlim = c(0,1))
lines(density(Results[ , 2, 3]), lty = 2) # SRMR.

# Describe
library(psych)
describe(Results[,,1])
describe(Results[,,2])
describe(Results[,,3])

# Including additional measures into the joint analysis with 'good fit' mask the distrunbances from CFI, RMSEA and SRMR.
# Hence, if you'd commit to using fit indices and their cutoffs, you could not generalize results from analysis with less measures to more measures.
# Instead you'd might very well be misguided I think.
# I think most importantly, in both scenarios, the correlations not accounted for in the model might as well be substantively insignificant correlations due to, for example, semantic similarity.
# In both scenarios, it remains ambiguous if we should 'reject' the specified model by indices: We'd need to inspect the residual correlations:

ResidualScenario1 <- lavaan::resid(sem(model = FitModel1, simulateData(TrueModel1, sample.nobs = 5000)))
library(qgraph)
qgraph(ResidualScenario1$cov)
# Now, is there an easy explanation why the residual correlations exist?
# In this hypothetical scenario, it might be possible to interpret the residual correlations (i.e., as semantical similarity, or just obvious similarity).



