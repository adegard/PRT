# PRT
PRT Machine learning 

R code for decision trees models uses “caret” package. Neural network uses “neuralnet” package. Standard trading indicators are calculated using “TTR” package. 

Note: You will find the original code sources for each techniques as references, but all code has been modified and can’t give any guarantee of success for trading.

1.Process of data Analysis: 
Data extraction: I used MT4 platform to export historical data in CSV format at prefered timeframe. The path to csv file must be indicated in the code.  
Determination of future price movement by shifting values to 5 periods in the future 
Calculation of custom and standard indicators (TTR)
Training of the model (classification, neural network or regression)
Printing / analysis of results

2.CLASSIFICATION MODEL for NON-PARAMETRIC PREDICTION

Filename: “Predictive RPART Price Modeling v3.0.R”

(code reference: https://www.quantinsti.com/blog/predictive-modeling-algorithmic-trading/)

The price value is transformed to non-parametric "class" of movement: UP, DOWN or NOTHING (possibile actions to BUY, SELL or NOTHING).
The file uses rpart training method to obtain decision trees, without pre processing to maintained values scales of each indicators.

PRO: rpart used directly is very fast. Random forest features selection skipped because too slow..
CONS: I omit to test the model on real data after training: I prefer to make it directly in “backtest” with  trading code like Prorealcode or MQL4

3.NEURAL NETWORK AND REGRESSION MODEL for PARAMETRIC PREDICTION

Filename “neuralnetR for PRT v1.2.R”

(code reference: https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/)

It use parametric values of price changes from now to 5 periods in the future.
The file uses neural network compared to multiple linear regression. 

PRO: regression is faster but works for parametric features (how to choose it?) 
Easier to classification because there is no need to transform values in “classes”.
CONS: neural network difficult to code in trading system. Features must be reduced at the minimum.
