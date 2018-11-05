## 3Dplot
## http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
## http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization#install-plot3d-package
## https://cran.r-project.org/web/packages/plot3D/plot3D.pdf 

## To load interact with XYZ data:
library(plot3D)
library(rgl)



setwd("D:/3D data analysis - interpretation/seedling")
mydata = read.csv("Seedling - model2.csv",sep = "\t")
X <- as.numeric(mydata[1:nrow(mydata),1])
Y <- as.numeric(mydata[1:nrow(mydata),2])
Z <- as.numeric(mydata[1:nrow(mydata),3])




plot(X,Y)
plot(X,Z)
plot(Y,Z)





## Excel example here






## or with more insights:
scatter3D(X,Y,Z)
plot3d(mydata,alpha=0.1)
plot3d(mydata) 
