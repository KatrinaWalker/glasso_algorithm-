
######## Apply GLASSO algorithm to a panel of financial returns ######## 

rm(list = ls())

# import GLASSO  -----------------------------------------------------------
source('glasso.R')

# Shape Data -----------------------------------------------------------
# raw data
amzn <- read.table("AMZN.csv", header = TRUE, sep = ",")
ba <- read.table("BA.csv", header = TRUE, sep = ",")
ibm <- read.table("IBM.csv", header = TRUE, sep = ",")
orcl <- read.table("ORCL.csv", header = TRUE, sep = ",")
aapl <- read.table("AAPL.csv", header = TRUE, sep = ",") 
nke <- read.table("NKE.csv", header = TRUE, sep = ",")

T1 <- nrow(amzn)
T2 <- nrow(ba)
T3 <- nrow(ibm)
T4 <- nrow(orcl)
T5 <- nrow(aapl)
T6 <- nrow(nke)

# inverts dataset
D1 <- amzn[ seq(T1,1,-1) , ]
D2 <- ba[ seq(T2,1,-1) , ]
D3 <- ibm[ seq(T3,1,-1) , ]
D4 <- orcl[ seq(T4,1,-1) , ]
D5 <- aapl[ seq(T5,1,-1) , ]
D6 <- nke[ seq(T6,1,-1) , ]

# constructs return dates
dates <- as.Date( as.character( D1[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D2[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D3[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D4[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D5[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D6[,1] ) , '%Y-%m-%d' )

# constructs prices
p1 <- D1[,ncol(D1)];
p2 <- D2[,ncol(D2)];
p3 <- D3[,ncol(D3)];
p4 <- D4[,ncol(D4)];
p5 <- D5[,ncol(D5)];
p6 <- D6[,ncol(D6)];

# constructs returns
amzn <- diff( log( p1 ) )*100
ba <- diff( log( p2 ) )*100
ibm <- diff( log( p3 ) )*100
orcl <- diff( log( p4 ) )*100
aapl <- diff( log( p5 ) )*100
nke <- diff( log( p6 ) )*100

# pack dates and returns in a data frame
returns <- cbind.data.frame(amzn, ba, ibm, orcl, aapl, nke)

# Constructs a covariance matrix with data to test -----------------------------------
M <- as.matrix(returns)

k <- ncol(M) #number of variables
n <- nrow(M) #number of subjects

#creates means for each column
M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(amzn),mean(ba),mean(ibm),mean(orcl),mean(aapl), mean(nke)) 

#creates a difference matrix
D <- M - M_mean

#creates the covariance matrix
Y <- (n-1)^-1*t(D) %*% D

# using the cov function for comparison 
Y2 <- cov(M) 

# Run GLASSO -----------------------------------
print(Theta_ident <- GLASSO(Y))
