library(shiny)
library(shinythemes)
library(forecast)
library(quantmod)
library(ggplot2)
library(tseries)
library(fontawesome) 

#UI
ui = fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(title = "Stock Prediction Dashboard",
             tabPanel("About",#About Page
                      h1("About Stock Prediction Dashboard"),
                      p("Welcome to the BGSU Stock Prediction Dashboard. This interactive tool provides stock analysis and prediction capabilities, enabling users to input stock ticker symbols and receive detailed forecasts and trend analyses."),
                      p("Utilize ARIMA-based forecasting models to predict future stock prices both with and without seasonal adjustments. The dashboard also offers visualizations like line charts and candlestick charts to illustrate stock movements, enhancing your financial decision-making process."),
                      p("Explore the 'Dashboard' tab to enter stock codes, generate forecasts, and download predictive data in CSV format. The 'Charts' tab provides dynamic charting capabilities to visualize historical price data with some technical indicators."),
                      p("***This Is Not Financial Advice. Do Your Own Research Before Making Any Trade Decision.***"),
                      
                      p("A monkey could do a regression - Dr. Walt Ryley")
             ),
             tabPanel("Dashboard",#Dashboard Page
                      sidebarPanel(
                        textInput("StockCode", "Enter Stock Ticker", value = "AAPL"),
                        actionButton("click", "Predict"),
                        br(),
                        downloadButton("downloadAutoArima", "Download Auto Arima CSV"),
                        downloadButton("downloadSeasonalArima", "Download Seasonal Arima CSV")
                      ),
                      mainPanel(
                        plotOutput("auto.arima"),
                        tableOutput("auto.arima1"),
                        plotOutput("arima.seasonal"),
                        tableOutput("arima.seasonal1")
                      )
             ),
             tabPanel("Charts",#Dashboard Page
                      plotOutput("lineChart"),
                      plotOutput("candleChart")
             )
  )
)


server = function(input, output) {
  observeEvent(input$click, {
    req(input$StockCode)  
    stockSymbol = input$StockCode
    stockData = getSymbols(Symbols = stockSymbol, src = "yahoo", from = "2016-01-01", auto.assign = FALSE) %>%
      as.data.frame() %>%
      `colnames<-`(c("Open", "High", "Low", "Close", "Volume", "Adj")) %>%
      transform(Close_MA = ma(Close, order = 7)) 
    
    #Download data
    output$downloadAutoArima = downloadHandler(
      filename = function() {
        paste("auto_arima_prediction_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        stockSymbol = input$StockCode
        if (nchar(stockSymbol) > 0) {
          stockData = stockData
          fit = auto.arima(stockData$Close, ic = "bic")
          fit.forecast = forecast(fit)
          write.csv(fit.forecast, file)
        }
      }
    )
    
    # Download handler for arima.seasonal1
    output$downloadSeasonalArima = downloadHandler(
      filename = function() {
        paste("seasonal_arima_prediction_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        stockSymbol = input$StockCode
        if (nchar(stockSymbol) > 0) {
          stockData = stockData
          tsData = ts(na.omit(stockData$v7_MA), frequency = 30)
          decomp = stl(tsData, s.window = "periodic")
          adjData = seasadj(decomp)
          fit = auto.arima(adjData, seasonal = TRUE)
          fcast = forecast(fit, h = 10)
          write.csv(fcast, file)
        }
      }
    )
    
    output$auto.arima = renderPlot({
      fit = auto.arima(stockData$Close, seasonal = FALSE, ic = "bic")
      forecastPlot = forecast(fit)
      plot(forecastPlot, main = paste("Non Seasonal Forecast for", stockSymbol))
    })
    
    output$auto.arima1 = renderTable({
      fit = auto.arima(stockData$Close, seasonal = FALSE, ic = "bic")
      forecast(fit, h = 20)
    })
    
    output$arima.seasonal = renderPlot({
      fit = auto.arima(stockData$Close,seasonal = TRUE, ic = "aic")
      forecastPlot = forecast(fit)
      plot(forecastPlot, main = paste("Seasonal Forecast for", stockSymbol))
    })
    
    output$arima.seasonal1 = renderTable({
      fit = auto.arima(stockData$Close, seasonal = TRUE)
      forecast(fit, h = 20)
    })
    
    output$lineChart = renderPlot({
      chartSeries(stockData[, c("Open", "High", "Low", "Close")], type = "matchsticks", TA=c(addRSI(n=14,maType = "EMA")))
    })
    
    output$candleChart = renderPlot({
      chartSeries(stockData[, c("Open", "High", "Low", "Close")], type = "matchsticks",TA=c(addMACD(),addBBands()))
    })
    
  })
  
}

# Run the application 
shinyApp(ui, server)
