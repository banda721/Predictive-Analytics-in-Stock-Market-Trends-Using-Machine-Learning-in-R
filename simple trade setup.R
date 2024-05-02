library(quantmod)
library(zoo)
library(forecast)
library(TTR)
library(tidyverse) 


shift=function(x,n){
  c(x[-(seq(n))], rep(NA,n))
}

# Fetch stock data using quantmod
symbol = "SPY"  # Example stock symbol
getSymbols("SPY",from = "2016-01-01")
barChart(SPY)
barChart(SPY, subset='last 28 days')
chartSeries(SPY, subset='last 365 days')
addMACD()
addBBands()

#set up dataframe 
data = data.frame(SPY)

#bullish candles
data$bull=1
data$bull=ifelse(data$SPY.Open>data$SPY.Close, 0, data$bull)

data$count=1
for(i in 2:nrow(data)) {  # Start from the second row
  if(data[i, "bull"] == data[i-1, "bull"]) {
    data[i, "count"] = data[i-1, "count"] + 1
  }
  else {
    data[i, "count"] = 1  # Initialize or reset the count for a new sequence
  }
}

head(data,20)

#mark what the next day was
data$nextcount=shift(data$count,1)

head(data,20)

mytable=table(data$count,data$nextcount)

mytable
percent_table=prop.table(mytable,1)

print.table(local({percent_table[percent_table==0]=NA;percent_table}))

#saving open close prices for trading
data$nextopen=shift(data$SPY.Open,1)
data$nextclose=shift(data$SPY.Close,1)
head(data,20)

#when count is 2, buy/sell the opposite way **can change the count based on risk preference and the table
Trades=subset(data,data$count==2)

#marking directions to trade
Trades$direction=1
Trades$direction=ifelse(Trades$bull==1,-1,Trades$direction)

#Setting cost
Trades$cost=0.002
amt=10000

#calculate profit or loss
Trades$profit=Trades$direction*(Trades$nextclose-Trades$nextopen)-Trades$cost



# Initialize starting capital
Trades$capital = 10000  # Start with $10000

Trades$shares = amt/Trades$nextopen  # Initialize the number of shares that can be bought
Trades$TProfit = Trades$shares*Trades$profit
Trades$balace=cumsum(Trades$TProfit)
plot(Trades$balace, xlab="Days", ylab="Profit", main="Trend Following Profit", type="l",col="green")
tail(Trades$balace,10)

Final_profit_pct=((tail(Trades$balace, 1))/amt)*100

Final_profit_pct

ProfitPCT_per_trade=Final_profit_pct/nrow(Trades)

Annualised_profit=((1+(ProfitPCT_per_trade/100))^365-1)*100

Annualised_profit
