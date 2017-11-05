#Predictive price modelling with Machine learning (rpart, random forest)
#original code source:
#https://www.quantinsti.com/blog/predictive-modeling-algorithmic-trading/
#modified by A.Degardin
#last update 05/11/17
library(quantmod); library(TTR); library(caret);library(corrplot);library(pROC);library(FSelector);
library(ggplot2)
library(xts)
library(zoo)

#path to csv file
wdir="C:\\Users\\degar_000\\Google Drive\\Trading\\R\\Predictive-Modeling-program"
setwd(wdir)

#INPUT PARAMETERS##########
LastEntries <- 4500 #max number 4000-10000;
maxrows <- 7000 #file max rows extraction
quantileNothingvalue<-0.45 #trials : 0.55-0.65 
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
Future_price =c(tail(df_stock$Last, -3),NA,NA,NA)#,NA,NA,NA,NA,NA)#,NA,NA,NA)
Future_price2 =c(tail(df_stock$Last, -7),NA,NA,NA,NA,NA,NA,NA)#,NA,NA,NA)#,NA,NA,NA)
price = Future_price - df_stock$Last
price2 = Future_price2 - df_stock$Last
hist(price2, breaks=40)
quartinf=quantile(price2,(1-quantileNothingvalue)/2 , type = 1, na.rm = TRUE)
quartsup=quantile(price2,1- (1-quantileNothingvalue)/2, type = 1, na.rm = TRUE)
abline(v=quartinf,col="red")
abline(v=quartsup,col="red")
class = ifelse((price2 >= quartsup & price>=0),"UP",
               ifelse((price2 < quartinf & price<0),"DOWN","NOTHING"))

cbind(freq=table(class), percentage=prop.table(table(class))*100)

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
#ADX5 <- ADX(df_stock[,c("Last")], n=5)[,4] ;ADX5_1 = c(NA,NA,head(ADX5,-2));ADX5_2 = c(NA,NA,head(ADX5,-2)) ;ADX5_4 = c(NA,NA,NA,NA,head(ADX5,-4)) ;
MACDI <-MACD(df_stock[,c("Last")], 12, 26, 9, maType="EMA" )[,1];MACDI1 = c(NA,NA,head(MACDI,-2));MACDI2 = c(NA,NA,head(MACDI,-2)) ;MACDI4 = c(NA,NA,NA,NA,head(MACDI,-4)) ;

#most important features with random forest############
#dataset<-NULL
#dataset = data.frame(class,
#                        WillR5,WillR51,WillR52,WillR10,WillR101,WillR102,
#                        RSI5, RSI10,#RSI51,RSI52,RSI101,RSI102,
#                        #ROC51,ROC52,
#                        #MOM5,MOM51,MOM52,
#                        #ATR5,ATR20,#ATR51,ATR52,ATR54,ATR201,ATR202, 
#                        RatioATR, ###
#                        bb5,bb51,bb52,bb54,
#                        bb10,bb101,bb102,bb104,
#                        PEMA5,PEMA51,PEMA52,PEMA54,EMA520, ###
#                        stoc,stoc1,stoc2, stoc4,
#                        #ADX51,ADX52,ADX54,
#                        #Ratio1, Ratio2, Ratio3, Ratio4, Ratio5, ####Ratio3##
#                        #Ratio6, Ratio7, Ratio8,Ratio9,Ratio10, Ratio98,Ratio11,
#                        #Ratio11, Ratio21, Ratio31, Ratio41, Ratio51,
#                        #Ratio61, Ratio71, Ratio81,Ratio91,Ratio101, Ratio981,Ratio111,
#                        MACDI,MACDI1,MACDI2,MACDI4,
#                        Gain, Gain1, Gain2, Gain4,
#                        PinBar, PinBar1) ### Gain4##
#dataset = na.omit(dataset)
## Selecting features using the random.forest.importance function from the FSelector package
###set.seed(5)
#weights = random.forest.importance(class~., dataset, importance.type = 1)
#print(weights)

set.seed(5)
#subset <- cutoff.k(weights, 5)
#f <- as.simple.formula(subset, "class")
#print(f)

#subset_rf = cutoff.k(weights, 10)
#print(subset_rf)

######################### DECISION TREE TRAINING #############
dataset_rf<-NULL
## Creating a dataframe using the selected features

dataset_rf = data.frame(class,
                        #WillR5,WillR51,WillR52,WillR10,WillR101,WillR102,
                        RSI5, RSI10,#RSI51,RSI52,RSI101,RSI102,
                        #ROC51,ROC52,
                        #MOM5,MOM51,MOM52,
                        #ATR5,ATR20,#ATR51,ATR52,ATR54,ATR201,ATR202, 
                        RatioATR, ###
                        bb5,bb51,bb52,bb54,
                        bb10,bb101,bb102,bb104,
                        PEMA5,PEMA51,PEMA52,PEMA54,EMA520, ###
                        stoc,stoc1,stoc2, stoc4,
                        #ADX51,ADX52,ADX54,
                        Ratio1, Ratio2, Ratio3, Ratio4, Ratio5, ####Ratio3##
                        Ratio6, Ratio7, Ratio8,Ratio9,Ratio10, Ratio98,Ratio11,
                        Ratio11, Ratio21, Ratio31, Ratio41, Ratio51,
                        Ratio61, Ratio71, Ratio81,Ratio91,Ratio101, Ratio981,Ratio111,
                        #MACDI,MACDI1,MACDI2,MACDI4,
                        Gain, Gain1, Gain2, Gain4,
                        PinBar, PinBar1) ### Gain4##

#dataset_rf = na.omit(dataset_rf)
dataset_rf<-dataset_rf[complete.cases(dataset_rf), ]
summary(dataset_rf)
# Resampling method used - 10-fold cross validation 
# with "Accuracy" as the model evaluation metric.
trainControl = trainControl(method="cv", number=10) #method="cv"
metric = "Accuracy" 

fit.cart<-NULL
# Classification and Regression Trees (CART)
set.seed(5)

fit.cart = train(dataset_rf[2:ncol(dataset_rf)], dataset_rf$class, method="rpart", 
                 metric=metric,trControl=trainControl)
fit.cart
fit.cart$finalModel
library(rattle)
fancyRpartPlot(fit.cart$finalModel, main=paste(mFileName1, "final model", sep=" "), type=3)

