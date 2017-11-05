#Predictive price modelling with Neural Network compared to Linear Regression 
#original code source:
#https://www.google.it/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwimjIHf2p3XAhWBuBoKHU-xD30QFggsMAA&url=https%3A%2F%2Fwww.r-bloggers.com%2Ffitting-a-neural-network-in-r-neuralnet-package%2F&usg=AOvVaw2OS14eK4Q81GuieJXwcqer
#modified by A.Degardin
#last update 05/11/17
# Set a seed
set.seed(500)
library(quantmod); library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);
library(ggplot2)
library(xts)
library(zoo)

#path to csv file
wdir="C:\\Users\\degar_000\\Google Drive\\Trading\\R\\Predictive-Modeling-program"
setwd(wdir)

#INPUT PARAMETERS##########
LastEntries <- 3000 #max number 4000-10000; +accuracy but less models
maxrows <- 4000 #file max rows extraction
mFileName1<-"EURUSD15.csv" # exported frome MT4 platform
###########################

df_stock1<-NULL

## Use set.seed function to ensure the results are repeatable
set.seed(5)

## Read the stock and index data from file1###########
nL1 <- length(readLines(mFileName1))
df_stock1 = read.csv(mFileName1, header=FALSE, skip=nL1-maxrows) # file from MT4 export 60'
colnames(df_stock1) = c("A","B","C","D","E","F","G") 
#str(df_stock1)
df_stock1$A=paste(df_stock1$A,df_stock1$B,sep=" ")

rownames(df_stock1) <- df_stock1$A #put date as index
#df_stock1$A <- NULL
df_stock1$B <- NULL

colnames(df_stock1) = c("Date","Open","High","Low","Last","Vol") 
#N <- length(df_stock1$Open)-LastEntries
#df_stock1<-tail(df_stock1, -N)
df_stock1<-df_stock1[complete.cases(df_stock1), ]

df_stock<-NULL
df_stock<-df_stock1
head(df_stock)
## Compute the price change for the stock and classify as UP/DOWN
Future_price =c(tail(df_stock$Last, -5),NA,NA,NA,NA,NA)#,NA,NA)#,NA,NA,NA)#,NA,NA,NA)
price = Future_price - df_stock$Last
#hist(price, breaks=40)

#Doji prices
O<-df_stock$Open ; O1= c(NA,head(O,-1))  ; O2= c(NA,NA,head(O,-2)) ;
C<-df_stock$Last ; C1= c(NA,head(C,-1))  ; C2= c(NA,NA,head(C,-2)) ;
H<-df_stock$High ; H1= c(NA,head(H,-1))  ; H2= c(NA,NA,head(H,-2)) ;
L<-df_stock$Low ; L1= c(NA,head(L,-1))  ; L2= c(NA,NA,head(L,-2)) ;
#Doji ratios
Ratio1<-C1/O1 ; Ratio11= c(NA,head(Ratio1,-1))
Ratio2<-C/O ; Ratio21= c(NA,head(Ratio2,-1))
Ratio3<-C/O1 ; Ratio31= c(NA,head(Ratio3,-1))   
Ratio4<-C1/O ; Ratio41= c(NA,head(Ratio4,-1))
Ratio5<-(C1-O1)/(O-C) ; Ratio51= c(NA,head(Ratio5,-1))
Ratio6<-(O-C)/(H-L) ; Ratio61= c(NA,head(Ratio6,-1))
Ratio7<-(C2-O2)/(O1-C1) ; Ratio71= c(NA,head(Ratio7,-1))
Ratio8<-(O1-C1)/(H1-L1) ; Ratio81= c(NA,head(Ratio8,-1))
Ratio9<-(H1-C1)/(H1-L1) ; Ratio91= c(NA,head(Ratio9,-1))
Ratio98<-Ratio9/Ratio8 ; Ratio981= c(NA,head(Ratio98,-1))
Ratio10<- ((H - L) / abs(C - O)); Ratio101= c(NA,head(Ratio10,-1))
Ratio11<- abs(O - L) / (H - L); Ratio111= c(NA,head(Ratio11,-1))

PinBar<-ifelse( ((H - L) > 3* abs(C - O)),
                ifelse( ((O - L) < 0.3*(H - L) || (C - L) < 0.3*(H-L)),-1,
                        ifelse( ((H - O) < 0.3*(H - L) || (H - C) < 0.3*(H-L)),1,
                                0)),0) ; PinBar1= c(NA,head(PinBar,-1))


#yearstarday gain
Gain = (df_stock$Last - df_stock$Open)/df_stock$Open ; Gain1 = c(NA,head(Gain,-1)) ; Gain2 = c(NA, NA,head(Gain,-2)) ; Gain4 = c(NA,NA,NA,NA,head(Gain,-4)) ;
# Force Index Indicator
#FI = (df_stock$Last - df_stock$Open) * df_stock$Vol ; FI1 = c(NA,head(FI,-1)) ; FI2 = c(NA, NA,head(FI,-2)) ; FI4 = c(NA,NA,NA,NA,head(FI,-4)) ;
#price on EMA
PEMA5 = df_stock$Last / EMA(df_stock[,c("Last")], 5);  PEMA51 = c(NA,head(PEMA5,-1)) ;PEMA52 = c(NA,NA,head(PEMA5,-2)) ;PEMA54 = c(NA,NA,NA,NA,head(PEMA5,-4)) ;
PEMA10 = df_stock$Last / EMA(df_stock[,c("Last")], 10);   PEMA101 = c(NA,head(PEMA10,-1)) ;PEMA102 = c(NA,NA,head(PEMA5,-2)) ;PEMA104 = c(NA,NA,NA,NA,head(PEMA10,-4)) ;

EMA520=EMA(df_stock[,c("Last")], 5)/EMA(df_stock[,c("Last")], 20);   EMA5201 = c(NA,head(EMA520,-1))

WillR5  = WPR(df_stock[,c("High","Low","Last")], n = 5)*(-100) ; WillR51 = c(NA,head(WillR5,-1)) ; WillR52 = c(NA, NA,head(WillR5,-2)) ;
WillR10  = WPR(df_stock[,c("High","Low","Last")], n = 10)*(-100) ; WillR101 = c(NA,head(WillR10,-1)) ; WillR102 = c(NA, NA,head(WillR10,-2)) ;

RSI5  = RSI(df_stock$Last, n = 5,maType="WMA") ;RSI51 = c(NA,head(RSI5,-1)) ;RSI52 = c(NA,NA,head(RSI5,-2)) ;RSI54 = c(NA,NA,NA,NA,head(RSI5,-4)) ;
RSI10 = RSI(df_stock$Last, n = 10,maType="WMA") ;RSI101 = c(NA,head(RSI10,-1)) ;RSI102 = c(NA,NA,head(RSI10,-2)) ;

#MFI5 = MFI(df_stock[,c("High","Low","Last")], df_stock[,"Vol"], n = 5)/100

# Price change Indicators (ROC and Momentum)
ROC5 = ROC(df_stock$Last, n = 5,type ="discrete")*100  ; ROC51 = c(NA,head(ROC5,-1)) ; ROC52 = c(NA,NA,head(ROC5,-2)) ;
MOM5 = momentum(df_stock$Last, n = 5, na.pad = TRUE) ; MOM51 = c(NA,head(MOM5,-1)) ;MOM52 = c(NA,NA,head(MOM5,-2)) ;

# Volatility signal Indicator (ATR)
ATR5 = ATR(df_stock[,c("High","Low","Last")], n = 5, maType="EMA")[,2] ; ATR51 = c(NA,head(ATR5,-1)) ;ATR52 = c(NA,NA,head(ATR5,-2)) ;ATR54 = c(NA,NA,NA,NA,head(ATR5,-4)) ;
ATR20 = ATR(df_stock[,c("High","Low","Last")], n = 20, maType="EMA")[,2] ; ATR201 = c(NA,head(ATR20,-1)) ;ATR202 = c(NA,NA,head(ATR20,-2)) ;
RatioATR = ATR5/ATR20
#other
bb5 = BBands(df_stock[,c("High","Low","Last")], n = 5, sd = 2, maType=EMA)[,4] ; bb51 = c(NA,head(bb5,-1)) ;bb52 = c(NA,NA,head(bb5,-2)) ;bb54 = c(NA,NA,NA,NA,head(bb5,-4)) ;bb56 = c(NA,NA,NA,NA,NA,NA,head(bb5,-6)) ;
bb10 = BBands(df_stock[,c("High","Low","Last")], n = 10, sd = 2, maType=EMA)[,4] ; bb101 = c(NA,head(bb10,-1)) ;bb102 = c(NA,NA,head(bb10,-2)) ; bb104 = c(NA,NA,NA,NA,head(bb10,-4)) 
stoc <- stoch(df_stock[,c("High","Low","Last")], nFastK = 14, nFastD = 3, nSlowD = 3)[,1] ; stoc1 = c(NA,head(stoc,-1)) ;stoc2 = c(NA,NA,head(stoc,-2)) ;stoc4 = c(NA,NA,NA,NA,head(stoc,-4)) 
ADX5 <- ADX(df_stock[,c("High","Low","Last")], n=5)[,4] #;ADX5_1 = c(NA,NA,head(ADX5,-2));ADX5_2 = c(NA,NA,head(ADX5,-2)) ;ADX5_4 = c(NA,NA,NA,NA,head(ADX5,-4)) ;
MACDI <-MACD(df_stock[,c("Last")], 12, 26, 9, maType="EMA" )[,1];MACDI1 = c(NA,NA,head(MACDI,-2));MACDI2 = c(NA,NA,head(MACDI,-2)) ;MACDI4 = c(NA,NA,NA,NA,head(MACDI,-4)) ;

######################### DECISION TREE TRAINING #############
dataset_nn<-NULL
## Creating a dataframe using the selected features

dataset_nn = data.frame(price,
                        WillR52,#WillR51,WillR52,WillR10,WillR101,WillR102,
                        RSI52,#RSI101,RSI102,#RSI5, #RSI10,#RSI51,
                        #ROC51,ROC52,
                        #MOM5,MOM51,MOM52,
                        #ATR5,ATR20,#ATR51,ATR52,ATR54,ATR201,ATR202, 
                        RatioATR, ###
                        #bb54,#bb51,bb52,bb54,
                        #bb10,#bb101,bb102,bb104,
                        PEMA54, #PEMA102,#,PEMA54,EMA520, ###
                        stoc4,#,stoc2, stoc4,
                        #ADX5,#,ADX52,ADX54,
                        Ratio1,#Ratio2, Ratio3, Ratio4,# Ratio5, ####Ratio3##
                        #Ratio6, Ratio7, Ratio8,Ratio9, Ratio98,Ratio11,
                        #Ratio10,Ratio11, Ratio21, Ratio31, Ratio41, Ratio51,
                        #Ratio61, Ratio71, Ratio81,Ratio91,Ratio101, Ratio981,Ratio111,
                        MACDI)#,MACDI4,#,MACDI2,MACDI4)
                        #Gain)#, Gain1, Gain2, Gain4)
                        #PinBar, PinBar1) ### Gain4##


dataset_nn<-dataset_nn[complete.cases(dataset_nn), ]
summary(dataset_nn)
data<-dataset_nn
# Check that no data is missing
apply(data,2,function(x) sum(is.na(x)))

# Train-test random splitting for linear model
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

# Fitting linear model
lm.fit<-NULL
lm.fit <- glm(price~., data=train)


# Predicted data from lm
pr.lm <- predict(lm.fit,test)

# Test MSE
MSE.lm <- sum((pr.lm - test$price)^2)/nrow(test)

#-------------------------------------------------------------------------------
# Neural net fitting

# Scaling data for the NN
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]

# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("price ~", paste(n[!n %in% "price"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

#str(nn$weights)
# Visual plot of the model
#plot(nn)

#nn$result.matrix
#head(nn$generalized.weights[[1]])
#gwplot(nn, selected.covariate="stoc")


# Predict
ncol(test_)
pr.nn <- compute(nn,test_[,2:ncol(test_)])
#View(test_)
# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(data$price)-min(data$price))+min(data$price)
test.r <- (test_$price)*(max(data$price)-min(data$price))+min(data$price)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))

# Plot predictions
par(mfrow=c(1,2))

plot(test$price,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$price,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


summary(lm.fit)

