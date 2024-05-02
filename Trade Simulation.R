# Load necessary libraries
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(tidyverse)
library(lubridate)
library(xts)

# data for functions
tickerSymbol = 'SPY'
data = data.frame(getSymbols(tickerSymbol, src="yahoo", from="2016-01-01", auto.assign=FALSE))
prices = Cl(data)
returns = diff(log(prices))

amt = 10000
################################################################################
#buy and hold strategy
BHS=(tail(prices, 1))/(head(prices, 1))*amt
print(paste("Final Amount:", BHS))
print(paste("Gain:", BHS-amt))

################################################################################
set.seed(123)  

# Simulate random buy and sell
run_random_trade_simulation = function(prices) {
  amt = 10000  # Initial amount
  buy_price = 0
  curr_holding = FALSE
  trades = list()
  
  for (date in 1:(length(prices) - 1)) {
    if (!curr_holding) {
      # Randomly decide to buy or not
      if (runif(1) < 0.5) {
        buy_price = prices[date]
        curr_holding = TRUE
        trades = append(trades, list(c('Buy', date, buy_price)))
      }
    } else {
      # Sell on the next day
      sell_price = prices[date + 1]
      ret = (sell_price - buy_price) / buy_price
      amt = amt * (1 + ret)
      curr_holding = FALSE
      trades = append(trades, list(c('Sell', date + 1, sell_price, ret)))
    }
  }
  
  return(list(final_amount = amt, trades = trades))
}

# Returns
result = run_random_trade_simulation(prices)

# Print the results
print(paste("Final Amount:", result$final_amount))
print(paste("Gain:", result$final_amount-amt))
print("Trades executed:")
print(result$trades)

#################################################################################

# Simulate trade based on improvement from the previous day
run_improvement_trade_simulation = function(prices) {
  amt = 10000  # Initial amount
  buy_price = 0
  curr_holding = FALSE
  trades = list()
  
  for (date in 2:length(prices)) {  # Start from the second day to compare with the first day
    if (!curr_holding) {
      # Buy only if today's price is higher than yesterday's
      if (prices[date] > prices[date - 1]) {
        buy_price = prices[date]
        curr_holding = TRUE
        trades = append(trades, list(c('Buy', date, buy_price)))
      }
    } else {
      # Sell on the next day
      sell_price = prices[date]
      ret = (sell_price - buy_price) / buy_price
      amt = amt * (1 + ret)
      curr_holding = FALSE
      trades = append(trades, list(c('Sell', date, sell_price, ret)))
    }
  }
  
  return(list(final_amount = amt, trades = trades))
}

# Example usage with hypothetical price data
result = run_improvement_trade_simulation(prices)

# Print the results
print(paste("Final Amount:", result$final_amount))
print(paste("Gain:", result$final_amount-amt))
print("Trades executed:")
print(result$trades)



#################################################################################

amt=10000

run_simulation = function(returns, prices, amt, model_order, trade_thresh, verbose=FALSE, plot=TRUE) {
  curr_holding = FALSE
  events_list = list()
  init_amt = amt
  
  for (date in 15:length(returns)) {
    if (curr_holding) {
      sell_price = prices[date]
      curr_holding = FALSE
      ret = (sell_price / buy_price)-1
      amt = amt * (1 + ret)
      events_list = append(events_list, list(c('s', date, ret)))
      
      if (verbose) {
        print(paste('Sold at $', sell_price))
        print(paste('Predicted Return: ', round(pred, 4)))
        print(paste('Actual Return: ', round(ret, 4)))
        print('=======================================')
      }
      next
    }
    
    curr_data = returns[1:date]
    
    if (is.vector(model_order)) {
      tryCatch({
        model = arima(curr_data, order=model_order)
        pred = predict(model, n.ahead=1)$pred
      }, error=function(e) {
        pred = trade_thresh - 1  
      })
    }
    
    if (!curr_holding && 
        (pred > trade_thresh || (is.numeric(trade_thresh) && runif(1) < trade_thresh))) {
      curr_holding = TRUE
      buy_price = prices[date]
      events_list = append(events_list, list(c('b', date)))
      
      if (verbose) {
        print(paste('Bought at $', buy_price))
      }
    }
  }
  
  if (verbose) {
    print(paste('Total Amount: $', round(amt, 2)))
  }
  
  return(amt)
}

acf(returns)
pacf(returns)
auto.arima(returns, ic="aic")

final_amount1 = data.frame(run_simulation(returns, prices, amt, c(1, 0, 0), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount1))
print(paste("Gain:", final_amount1-amt))

final_amount2 = data.frame(run_simulation(returns, prices, amt, c(0, 0, 3), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount2))
print(paste("Gain:", final_amount2-amt))

final_amount3 = data.frame(run_simulation(returns, prices, amt, c(0, 0, 1), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount3))
print(paste("Gain:", final_amount3-amt))

final_amount4 = data.frame(run_simulation(returns, prices, amt, c(2, 1, 0), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount2))
print(paste("Gain:", final_amount2-amt))

final_amount5 = data.frame(run_simulation(returns, prices, amt, c(2, 0, 0), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount5))
print(paste("Gain:", final_amount5-amt))

final_amount6 = data.frame(run_simulation(returns, prices, amt, c(0, 0, 2), 0.05, verbose=TRUE))
print(paste("Final Amount:", final_amount6))
print(paste("Gain:", final_amount6-amt))











