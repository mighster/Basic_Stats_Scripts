## step-by-step illustration 11.3 
## y = Xb + Zu + e
## Table_1 for Table 11.1
## Table_2 for Coefficient of Coancestry on P270

Table_1 <- read.table('C:/jmyu/Documents/PB&G_Curriculum/AGRON621/2014/Mixed_Model_example/Table113_1', header = T, sep = "\t");
Table_2 <- read.table('C:/jmyu/Documents/PB&G_Curriculum/AGRON621/2014/Mixed_Model_example/Table113_2', header = T, sep = "\t");

Table_1
Table_2

y_vector <- Table_1$Grain_yield;
Y <- as.matrix(y_vector);

Y

Cultivar <- Table_1$Cultivar
Uni_Cultivar <- colnames(Table_2[-1]);

Uni_Number <- unique(Table_1$Number);

### VR/VA is the ratio of VR to VA from P273
VrByVa <- 5;

## X is the matrix for fixed effect
Uni_Enviroment <- unique(Table_1$Designation);

## initiate X matrix
X <- matrix(rep(0, length(y_vector) * length(Uni_Enviroment)), ncol = length(Uni_Enviroment) );
for (i in 1:length(Uni_Enviroment)) {
 order_in_y <- which(Table_1$Designation == Uni_Enviroment[i]);
 X[order_in_y, i] <- 1;
}
X

## Z is the matrix for randome effect
## initiate Z matrix
Z <- matrix(rep(0, length(y_vector)*length(Uni_Cultivar)), ncol = length(Uni_Cultivar));
### bulid Z matrix
for (i in 1:length(Uni_Cultivar)) {
  order_in_y <- which(Table_1$Cultivar == Uni_Cultivar[i]);
  Z[order_in_y, i] <- 1;
}
Z;

## A matrix for additive relationship
A <- 2 * as.matrix(Table_2[, -1]);
A <- matrix(as.vector(A), ncol = length(Uni_Cultivar)); ### remove the name of Cultivar
A
## R matrix
R <- diag(length(y_vector));
for (i in 1:length(y_vector)) {
  value <- 1 / Table_1$Number[i]
  R[i, i ] <-  value;
}
## inverse of R
R_1 <- solve(R);

### First part in P271: X'*R_1*X
XpR_1X <- t(X) %*% R_1 %*% X;
XpR_1X 
## Second part in P272: Z'R_1X
ZpR_1X <- t(Z) %*% R_1 %*% X;
ZpR_1X;
## Third part in P272: X'R_1Z
XpR_1Z <- t(ZpR_1X) ### or XpR_1Z <- t(X) %*% R_1 %*% Z
XpR_1Z
## Fourth part in P272 Z'R_1Z
ZpR_1Z <- t(Z) %*% R_1 %*% Z;
ZpR_1Z
 ## P273 A_1(VrByVa)
A_1VrByVa <- solve(A) * VrByVa;
A_1VrByVa
ZpR_1ZA_1VrByVa <- ZpR_1Z + A_1VrByVa;

## Fifth part in P274 
XpR_1Y <- t(X) %*% R_1 %*% Y;
XpR_1Y

## Sixth part in P275
ZpR_1Y <- t(Z) %*% R_1 %*% Y;
ZpR_1Y

### Left hand part
L_1 <- rbind(XpR_1X, ZpR_1X);
L_2 <- rbind(XpR_1Z, ZpR_1ZA_1VrByVa);
Lh <- cbind(L_1, L_2);
### Right hand part
Rh <- rbind(XpR_1Y, ZpR_1Y);
## bu matrix
LR <- solve(Lh) %*% Rh;
b <- LR[1:length(Uni_Enviroment),];
b
u <- LR[-(1:length(Uni_Enviroment)),];
u
