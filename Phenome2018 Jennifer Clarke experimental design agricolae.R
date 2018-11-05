library(agricolae)
designs <- apropos("design")
print(designs)
designs

data("sweetpotato")
str(sweetpotato)
trt <- levels(sweetpotato$virus)
r <- c(rep(3,4))
outdesign1 <- design.crd(trt,r,serie=0,seed=2020)
book1 <- outdesign1$book
head(book1)
write.table(book1,'crd.txt', row.names=FALSE,sep="")




data(grass)
str(grass)

trt <- levels(grass$trt)
r <-  12
outdesign2 <- design.rcbd(trt,r,serie=0,seed=2020)
outdesign2
book2 <- outdesign2$book


str(design.bib) 
function(trt,k,r=NULL, serie=2, seed = 0, kinds = "Super-Duper", maxRep = 20, randomization = TRUE)
