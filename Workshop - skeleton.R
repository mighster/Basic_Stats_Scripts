## skeleton example in 2D
## https://cran.r-project.org/web/packages/mmand/README.html#skeletonisation 

library(loder)
library(mmand)
B <- readPng(system.file("images", "B.png", package="mmand"))
k <- shapeKernel(c(3,3), type="diamond")
display(B)
display(skeletonise(B,k,method="lantuejoul"), col="red", add=TRUE)
display(skeletonise(B,k,method="beucher"), col="green", add=TRUE)
display(skeletonise(B,k,method="hitormiss"), col="blue", add=TRUE)

