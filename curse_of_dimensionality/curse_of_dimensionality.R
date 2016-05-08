# curse_of_dimensionality.R
# author: Bradley Mitchell

library(ggplot2)
library(grid)
library(gridExtra)

# Get distances between m points in n dimensions
getDistances <- function(m,n){
  M <- matrix(runif(n*m), ncol=n)
  Mdist <- M %*% t(M)
  Mdist <- sqrt(Mdist)
  return(as.vector(Mdist))
}

p2 <- qplot(getDistances(1000,2), xlab='Distance in 2D', ylab='Count')
p3 <- qplot(getDistances(1000,3), xlab='Distance in 3D', ylab='Count')
p7 <- qplot(getDistances(1000,7), xlab='Distance in 7D', ylab='Count')
p49 <- qplot(getDistances(1000,49), xlab='Distance in 49D', ylab='Count')

grid.arrange(p2, p3, p7, p49)

meanVec <- rep(0,999)
sdVec <- rep(0,999)

# Calculate mean and standard deviation of distances for each number of dimensions
for(i in 2:1000){
  vdist <- getDistances(1000,i)
  meanVec[i-1] <- mean(vdist)
  sdVec[i-1] <- sd(vdist)
  print(i)
}

mydata <- data.frame(meandist=meanVec, sddist=sdVec)

ggplot(data=mydata, aes(x=c(2:1000), y=mydata$meandist)) + geom_line() + 
  xlab('Number of Dimensions') + ylab('Mean Distance') + 
  ggtitle('Mean distance between random points with \n increasing number of dimensions')

ggplot(data=mydata, aes(x=c(2:1000), y=mydata$sddist)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
  xlab('Number of Dimensions') + ylab('Standard Deviation of Distance') + 
  ggtitle('Standard deviation of distance between random points \n with increasing number of dimensions')

# Volume of a hypersphere with n dimensions and radius r
nballVolume <- function(n, r){
  m <- n/2
  a <- pi^(m)
  b <- gamma(m+1)
  c <- r^n
  return((a/b)*c)
}

nbvol <- rep(0,18)
for (i in 3:20){
  nbvol[i-2] <- nballVolume(i,0.5)
}

mydata2 <- data.frame(num_dims=c(3:20), ratio=nbvol)
ggplot(data=mydata2, aes(x=mydata2$num_dims, y=mydata2$ratio)) + geom_line() +
  xlab('Number of Dimensions') + ylab('Ratio of Volumes') + 
  ggtitle('Ratio of volumes of hypersphere to hypercube \n with increasing number of dimensions')