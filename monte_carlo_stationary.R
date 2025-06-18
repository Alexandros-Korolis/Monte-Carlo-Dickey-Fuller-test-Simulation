##### Unit Root test - Dickey - Fuller - Monte Carlo #####
install.packages("ggplot2")
library(ggplot2)
install.packages("forecast")
library(forecast)

dev = c()

# Repeat 1000 times
for (n in seq(1:1000)){
  # Step 1: Create data from a random walk time series
  sample_size = 50
  time = c(0) # Initial time State
  series = c(0) # Initial timeseries state
  
  for (i in seq(1:sample_size)) {
    time = c(time,i)
    series = c(series,series[length(series)]+rnorm(1,mean = 0,sd=1))
  }
  
  data = data.frame(time = time, series = series)
  
  # Convert to timeseries data
  ts_data = ts(data = data$series, start = 0, frequency = 1)
  
  # Step 2: Estimate an AR(1) model
  model = tryCatch(arima(ts_data, order = c(1,0,0),method = "ML"),error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(model)) {
  # Step 3 : Compute test statistic   
    ar1 = unname(model$coef[1])-1
    se = unname(sqrt(diag(model$var.coef))[1])
    if (!is.na(ar1) && !is.na(se)) {
  # Store Values of test statistic
      dev = c(dev,ar1/se)
    }
  }
}

# Critical Values  
critical_values = quantile(dev, probs = c(0.01, 0.05, 0.10))


# Implementation  

# Simulate a random Walk 
sample_size = 50
time = c(0) 
series = c(0) 

for (i in seq(1:sample_size)) {
  time = c(time,i)
  series = c(series,series[length(series)]+rnorm(1,mean = 0,sd=1))
}

data = data.frame(time = time, series = series) # Store to dataframe

model = arima(ts_data, order = c(1,0,0),method = "ML") # AR(1)
Q = (unname(model$coef[1])-1)/unname(sqrt(diag(model$var.coef))[1])

if (Q<=critical_values[2]) {
    print("Reject Null Hypothesis")
} else {
  print("Fail to Reject the Null Hypothesis")
}








