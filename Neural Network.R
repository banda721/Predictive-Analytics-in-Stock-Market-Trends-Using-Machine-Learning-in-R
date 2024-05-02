library(neuralnet)
library(quantmod)
library(TTR)
library(dplyr)
library(ggplot2)
library(Metrics)

# Unscale Function
unscale = function(scaled_data, mean, sd) {
  (scaled_data * sd) + mean
}

#Function for RMSE
rmse = function(actual, predicted) {
  sqrt(mean((predicted - actual) ^ 2))
}



stock = "SPY"
lagging = 3
layers = c(10, 10, 10)
threshold = 0.01
seed = 2

stock_data = getSymbols(stock, env = globalenv(), src = "yahoo", from = "2016-01-01", auto.assign = FALSE)
mean(stock_data$SPY.Adjusted)
print(head(stock_data))
print(colnames(stock_data))


#####################################Training##############################################
print(colnames(stock_data))
scaled_features = list()



for (feature in c("Open", "High", "Low", "Close", "Volume")) {
  column_name = paste0(stock, ".", feature) 
  
  # Loop to create each lagged version of the data
  for (lag in 1:lagging) {
    lagged_data = Lag(stock_data[, column_name], k = lag)  
    lagged_name = paste0(feature, "_Lag", lag)  
    colnames(lagged_data) = lagged_name  
    
    # Scale the lagged data
    scaled_data = scale(lagged_data)
    scaled_name = paste0(lagged_name, "_Scaled")  
    
    scaled_features[[scaled_name]] = scaled_data 
  }
}
names(scaled_features)


#stock_data$Openscl=scale(stock_data$SPY.Open)
#stock_data$Closescl=scale(stock_data$SPY.Close)
stock_data$Lowscl=scale(stock_data$SPY.Low)
stock_data$Highscl=scale(stock_data$SPY.High)
#stock_data$Adjscl=scale(stock_data$SPY.Adjusted)

combined_data = do.call(cbind, scaled_features)
#combined_data$Openscled=stock_data$Openscl
#combined_data$Closescl=stock_data$Closescl
combined_data$Lowscl=stock_data$Lowscl
combined_data$Highscl=stock_data$Highscl
#combined_data$Adjscl=stock_data$Adjscl
combined_data = na.omit(combined_data)
names(combined_data)



print(head(combined_data))
length(combined_data)
set.seed(seed)
index = round(0.90 * nrow(combined_data))
train_data = combined_data[1:index, ]
test_data = combined_data[(index + 1):nrow(combined_data), ]

length(train_data)
length(test_data)

# Identify the target columns based on actual data
target_cols = c("Lowscl", "Highscl")
if (!all(target_cols %in% colnames(train_data))) {
  stop("Target columns not found in the training data.")
}

formula_str = paste(paste(target_cols, collapse = " + "), "~ .")
formula = as.formula(formula_str)

head(train_data)

#neuralnet model **might take sometime to run
nn = neuralnet(formula, train_data, hidden = layers, algorithm = "rprop+", stepmax = 1e+06, threshold = threshold)
print(nn$result.matrix)
head(nn$result.matrix,20)
plot(nn)
last_closing_price = tail(stock_data[, paste0(stock, ".Close")], 1)
print(last_closing_price)

######################################Testing##########################################


names(scaled_data)
names(scaled_features)

names(combined_data)

# Selecting the correct columns for testing
testDataMatrix = as.matrix(combined_data[(index + 1):nrow(combined_data), 
                                          c("Open_Lag1", "Open_Lag2", "Open_Lag3", 
                                            "High_Lag2", "High_Lag3", "Low_Lag2", 
                                            "Low_Lag3", "Close_Lag1", "Close_Lag2", 
                                            "Close_Lag3", "Volume_Lag1", "Volume_Lag2", "Volume_Lag3",
                                             "Lowscl","Highscl")])
head(testDataMatrix)


# neuralnet prediction
predictions = neuralnet::compute(nn, testDataMatrix)
predictedValues = predictions$net.result
head(predictedValues)


# Getting the low prediction and the the high prediction
predictedLow = predictedValues[, 1]
length(predictedLow)
head(predictedLow)
predictedHigh = predictedValues[, 2]
length(predictedHigh)
head(predictedHigh)



# Getting actual scaled data from test dataset,
actualLowPrices = test_data[, "Lowscl"]  
length(actualLowPrices)
head(actualLowPrices)
actualHighPrices = test_data[, "Highscl"]
length(actualHighPrices)
head(actualHighPrices)

# Error metrics
mseLow = mean((predictedLow - actualLowPrices)^2)
mseHigh = mean((predictedHigh - actualHighPrices)^2)

# Printing the MSE
print(paste("MSE for Low Prices:", mseLow))
print(paste("MSE for High Prices:", mseHigh))

#MAE
mae = mean(abs(predictedLow - actualLowPrices))
maeh = mean(abs(predictedHigh - actualHighPrices))

## Printing the MAE
print(paste("MAE Low:", mae))
print(paste("MAE High:", maeh))

#RMSE
cat("The RMSE for the Low Price values is:", sqrt(mseLow), "\n")
cat("The RMSE for the High Price values is:", sqrt(mseHigh), "\n")


valueData = data.frame(
  predictedLow = predictedLow,
  predictedHigh = predictedHigh,
  actualLowPrices = actualLowPrices,
  actualHighPrices = actualHighPrices
)

valueMatrix = as.matrix(valueData)

#valueMatrix = ts(cbind(predictedLow, predictedHigh, actualLowPrices, actualHighPrices))
head(valueMatrix)
colnames(valueMatrix) = c("Predicted Low", "Predicted High", "Actual Low", "Actual High")
print(head(valueMatrix))


#plots
ts.plot(ts(valueMatrix[, "Predicted Low"]), ts(valueMatrix[, "Actual Low"]), 
        gpars = list(col = c("red", "blue")), lty = 1, main = "Testing Performance Low Price (Scaled)")
legend("bottomright", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty = 1)

ts.plot(ts(valueMatrix[, "Predicted High"]), ts(valueMatrix[, "Actual High"]), 
        gpars = list(col = c("red", "blue")), lty = 1, main = "Testing Performance High Price (Scaled)")
legend("bottomright", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty = 1)


################################Trading Simulation :)####################################################################

#changing the names of actual prices
colnames(stock_data)[2] = "Low"
colnames(stock_data)[3] = "High"



stock_data_lag=list()

stock_data_lag$Low_Lag1=lag(stock_data$Low,1)
stock_data_lag$High_Lag1=lag(stock_data$High,1)

# mean and standard deviation for 'Low_Lag1' and 'High_Lag1' for unscale
low_mean = mean(stock_data_lag$Low_Lag1, na.rm = TRUE)
low_sd = sd(stock_data_lag$Low_Lag1, na.rm = TRUE)

high_mean = mean(stock_data_lag$High_Lag1, na.rm = TRUE)
high_sd = sd(stock_data_lag$High_Lag1, na.rm = TRUE)


# # unscaling the predictions using the 'unscale' function
length(valueData$predictedLow)

valueData_Unscaled = data.frame(
  predictedLow = unscale(valueData$predictedLow, low_mean, low_sd),
  predictedHigh = unscale(valueData$predictedHigh, high_mean, high_sd),
  actualLowPrices = tail(stock_data$Low,length(valueData$predictedLow)),
  actualHighPrices = tail(stock_data$High,length(valueData$predictedLow))
)

colnames(valueData_Unscaled)[3] = "actualLowPrices"
colnames(valueData_Unscaled)[4] = "actualHighPrices"

valueMatrix1 = as.matrix(valueData_Unscaled)

#Plots
ts.plot(ts(valueMatrix1[, "predictedLow"]), ts(valueMatrix1[, "actualLowPrices"]), 
        gpars = list(col = c("red", "blue")), lty = 1, main = "Testing Performance Low Price (Unscaled)")
legend("bottomright", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty = 1)

ts.plot(ts(valueMatrix1[, "predictedHigh"]), ts(valueMatrix1[, "actualHighPrices"]), 
        gpars = list(col = c("red", "blue")), lty = 1, main = "Testing Performance High Price (Unscaled)")
legend("bottomright", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty = 1)


colnames(valueMatrix1) = c("predictedLow", "predictedHigh", "actualLow", "actualHigh")


#Values for trading
capital = 10000
variance = 0.01
cost = 0.002
num_stocks = 0
short_stocks = 0

# Trading simulation loop
for(i in 1:nrow(valueMatrix1)) {
  predicted_low = valueMatrix1[i, "predictedLow"]
  actual_low = valueMatrix1[i, "actualLow"]
  actual_high = valueMatrix1[i, "actualHigh"]
  
  margin_low = c((1 - variance) * predicted_low, (1 + variance) * predicted_low)
  
  # Buy condition - entry for long position, using 80% of capital
  if(actual_low <= margin_low[2] && capital > 0 && num_stocks == 0) {
    buy_capital_to_use = capital * 0.8
    num_buy_stocks = floor(buy_capital_to_use / margin_low[2])
    cost_of_buying = num_buy_stocks * margin_low[2]
    capital = capital - cost_of_buying
    num_stocks = num_stocks + num_buy_stocks
    purchase_price = margin_low[2]
    print(paste("Day", i, ": Bought", num_buy_stocks, "stocks at", margin_low[2]))
  }
  
  # Sell condition - sell if there is a % loss (define loss %)
  if(num_stocks > 0 && actual_high < purchase_price * 0.925) {
    sell_amount = num_stocks * actual_high
    num_sell_stocks = num_stocks  # Decide to sell all stocks in this scenario
    revenue_from_sale = num_sell_stocks * actual_high
    fee = revenue_from_sale * cost
    capital = capital + (revenue_from_sale - fee)
    num_stocks = 0  # All stocks sold
    print(paste("Day", i, ": Sold", num_sell_stocks, "stocks at", actual_high, "due to 5% loss, Capital after selling:", capital, "with a fee of", fee))
  }
}

# Final trades for any remaining positions at the end of the period
if(num_stocks > 0) {
  revenue_from_final_sale = num_stocks * valueMatrix1[nrow(valueMatrix1), "actualHigh"]
  capital = capital + revenue_from_final_sale
  print(paste("End of the period: Sold remaining", num_stocks, "stocks at", valueMatrix1[nrow(valueMatrix1), "actualHigh"], "Capital is now", capital))
  num_stocks = 0
}


# Calculate and print profit or loss
profit_loss = capital - 10000
print(profit_loss)

percentage_change = (profit_loss / 10000) * 100
annualised_return=percentage_change/length(valueMatrix1[,1])*365

# Display final results including the number of days stocks were held
print(paste("Started with capital: ", 10000))
print(paste("Ended with capital: ", capital))
print(paste("Profit/Loss: ", profit_loss))
print(paste("Percentage change: ", round(percentage_change, 2), "%", sep = ""))
print(paste("Number of days stocks were held: ", length(valueData$predictedLow)))
print(paste("Annualised Return: ", round(annualised_return, 2), "%", sep = ""))


#Holding period return for the training period
Holding_Return=(valueMatrix1[i,4]/valueMatrix1[1,4]-1)*100
length(valueMatrix1[,1])
annualised_BHreturn=Holding_Return/(length(valueMatrix1[,1]))*365

print(paste("Buy and Hold Return: ", round(Holding_Return,2), "%", sep = ""))
print(paste("Annual Buy and Hold Return: ", round(annualised_BHreturn, 2), "%", sep = ""))

######################################################################################



