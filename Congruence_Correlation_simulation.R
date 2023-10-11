# Congruence vs. pearson r

# scenario 1: Perfect similarity with added noise, constant loadings
CongVSCor <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- rep( .7, times = 10 ) + runif( 10, -.1,.1 ) # Perfectly similarity, constant loadings, with added uniform noise.
  loadings2 <- rep( .7, times = 10 ) + runif( 10, -.1,.1 )
  
  
  CongVSCor[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                              cor( loadings1, loadings2 ), 
                                              distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                ncol = 3, nrow = 1 )
  
}

plot(density(CongVSCor[,2]), xlim = c(-1,1), col = "blue") # Pearson correlation similarity does not tell anything about the true similarity (perfect similarity).
lines(density(CongVSCor[,1]), xlim = c(-1,1), col = "red") # Congruence identifies the true similarity pretty well it seems.
lines(density(CongVSCor[,3]), xlim = c(-1,1), col = "black") # Distance was between 0 and .5.


# scenario 2: Perfect similarity with added noise, non-constant loadings
CongVSCor2 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor2 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- seq( .85,.55, length.out = 10 ) + runif( 10, -.1,.1 ) # Perfectly similar, but non-constant, loadings with added noise.
  loadings2 <- seq( .85,.55, length.out = 10 ) + runif( 10, -.1,.1 ) # 
  
  
  CongVSCor2[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                    cor( loadings1, loadings2 ), 
                                    distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                 ncol = 3, nrow = 1 )
  
}

plot(density(CongVSCor2[,2]), xlim = c(-1,1), col = "blue") # Pearson works better, but still fails decisively.
lines(density(CongVSCor2[,1]), xlim = c(-1,1), col = "red") # Congruence works correctly.
lines(density(CongVSCor2[,3]), xlim = c(-1,1), col = "black") # This doesn't change.

# scenario 3: Minor dissimilarity with added noise, non-constant loadings
CongVSCor3 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor3 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- seq( .85,.60, length.out = 10 ) + runif( 10, -.1,.1 ) # Minor dissimilarity, non-constant, loadings with added noise.
  loadings2 <- seq( .85,.55, length.out = 10 ) + runif( 10, -.1,.1 ) # 
  
  
  CongVSCor3[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe(CongVSCor3)
plot(density(CongVSCor3[,2]), xlim = c(-1,1), col = "blue") # Very similar to the previous scenario. It did not change markedly.
lines(density(CongVSCor3[,1]), xlim = c(-1,1), col = "red") # No change.
lines(density(CongVSCor3[,3]), xlim = c(-1,1), col = "black") # No change.


# scenario 4: Marked dissimilarity with added noise, non-constant loadings
CongVSCor4 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor4 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- seq( .85,.70, length.out = 10 ) + runif( 10, -.1,.1 ) # Marked dissimilarity, non-constant, loadings with added noise.
  loadings2 <- seq( .85,.55, length.out = 10 ) + runif( 10, -.1,.1 ) # 
  
  
  CongVSCor4[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe(CongVSCor4)
plot(density(CongVSCor4[,2]), xlim = c(-1,1), col = "blue") # Very similar to the previous scenario. It did not change markedly.
lines(density(CongVSCor4[,1]), xlim = c(-1,1), col = "red") # No change.
lines(density(CongVSCor4[,3]), xlim = c(-1,1), col = "black") # No change.

# scenario 5: Severe dissimilarity with added noise, non-constant loadings
CongVSCor5 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor5 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- seq( .85,.8, length.out = 10 ) + runif( 10, -.1,.1 ) # Severe dissimilarity, non-constant, loadings with added noise.
  loadings2 <- seq( .85,.55, length.out = 10 ) + runif( 10, -.1,.1 ) # 
  
  
  CongVSCor5[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe(CongVSCor5)
plot(density(CongVSCor5[,2]), xlim = c(-1,1), col = "blue") # Very similar to the previous scenario. It did not change markedly.
lines(density(CongVSCor5[,1]), xlim = c(-1,1), col = "red") # No change.
lines(density(CongVSCor5[,3]), xlim = c(-1,1), col = "black") # No change.

# scenario 6: Severe dissimilarity with added noise, non-constant loadings, loadings grow in different direction.
CongVSCor6 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor6 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- seq( .85,.8, length.out = 10 ) + runif( 10, -.1,.1 ) # Growth in different directions.
  loadings2 <- seq( .55,.85, length.out = 10 ) + runif( 10, -.1,.1 ) # 
  
  
  CongVSCor6[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe( CongVSCor6 )
plot(density(CongVSCor6[,2]), xlim = c(-1,1), col = "blue") # Very similar to the previous scenario. It did not change markedly.
lines(density(CongVSCor6[,1]), xlim = c(-1,1), col = "red") # No change.
lines(density(CongVSCor6[,3]), xlim = c(-1,1), col = "black") # No change.

# scenario 7: Completely random loadings.
CongVSCor7 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor7 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- runif(10, .0,.9)  # Random loadings over different intervals.
  loadings2 <- runif(10, .0,.3) 
  
  
  CongVSCor7[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe( CongVSCor7 )
plot(density(CongVSCor7[,2]), xlim = c(-1,1), col = "blue") # Correlation seems to give a good result.
lines(density(CongVSCor7[,1]), xlim = c(-1,1), col = "red") # Congruence fails utterly.
lines(density(CongVSCor7[,3]), xlim = c(-1,1), col = "black") # Distance is large.

# scenario 8: Completely random loadings, no loadings for the other factor.
CongVSCor8 <- matrix( nrow = 1000, ncol = 3, 0 )
colnames( CongVSCor8 ) <- c( "Congruence", "Pearson", "Distance" )
for ( i in 1:1000 ) {
  
  loadings1 <- runif(10, .3,.9)  # Random loadings over different intervals.
  loadings2 <- runif(10, -.5,.5) # Other loadings are negative.
  
  
  CongVSCor8[ i, 1:3 ] <- matrix( c( fa.congruence( loadings1, loadings2 )[1,1], 
                                     cor( loadings1, loadings2 ), 
                                     distance( matrix(loadings1), matrix(loadings2) )[1,1,drop=T] ),
                                  ncol = 3, nrow = 1 )
  
}

describe( CongVSCor8 )
plot(density(CongVSCor8[,2]), xlim = c(-1,1), col = "blue") # 
lines(density(CongVSCor8[,1]), xlim = c(-1,1), col = "red") # HERE congruence works.
lines(density(CongVSCor8[,3]), xlim = c(-1,1), col = "black") #