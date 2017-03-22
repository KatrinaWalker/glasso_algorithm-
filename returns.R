#########################
# Returns from Data     #
#########################

# Import Data  -----------------------------------------------------------

dir <- "~/glasso_algorithm-"
setwd(dir)

# Raw data
amzn <- read.table("data/AMZN.csv", header = TRUE, sep = ",")
ba <- read.table("data/BA.csv", header = TRUE, sep = ",")
ibm <- read.table("data/IBM.csv", header = TRUE, sep = ",")
orcl <- read.table("data/ORCL.csv", header = TRUE, sep = ",")
aapl <- read.table("data/AAPL.csv", header = TRUE, sep = ",") 
nke <- read.table("data/NKE.csv", header = TRUE, sep = ",")

T1 <- nrow(amzn)
T2 <- nrow(ba)
T3 <- nrow(ibm)
T4 <- nrow(orcl)
T5 <- nrow(aapl)
T6 <- nrow(nke)

# Inverts dataset
D1 <- amzn[ seq(T1,1,-1) , ]
D2 <- ba[ seq(T2,1,-1) , ]
D3 <- ibm[ seq(T3,1,-1) , ]
D4 <- orcl[ seq(T4,1,-1) , ]
D5 <- aapl[ seq(T5,1,-1) , ]
D6 <- nke[ seq(T6,1,-1) , ]

# Constructs return dates
dates <- as.Date( as.character( D1[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D2[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D3[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D4[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D5[,1] ) , '%Y-%m-%d' )
dates <- as.Date( as.character( D6[,1] ) , '%Y-%m-%d' )

# Construct prices
p1 <- D1[,ncol(D1)];
p2 <- D2[,ncol(D2)];
p3 <- D3[,ncol(D3)];
p4 <- D4[,ncol(D4)];
p5 <- D5[,ncol(D5)];
p6 <- D6[,ncol(D6)];

# Construct returns
amzn <- diff( log( p1 ) )*100
ba <- diff( log( p2 ) )*100
ibm <- diff( log( p3 ) )*100
orcl <- diff( log( p4 ) )*100
aapl <- diff( log( p5 ) )*100
nke <- diff( log( p6 ) )*100

# Pack dates and returns in a data frame
Y <- cbind.data.frame(amzn, ba, ibm, orcl, aapl, nke)