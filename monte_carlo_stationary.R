##### Monte Carlo Προσομοίωση για έλεγχο μοναδιαίας ρίζας #####
install.packages("ggplot2")
library(ggplot2)
install.packages("forecast")
library(forecast)

dev = c()
for (n in seq(1:1000)){
  # Αρχικές Συνθήκες (y_t = y_t-1 + ε_t), Προσομοίωση τυχαίου περίπατου
  sample_size = 50
  time = c(0)
  series = c(0)
  
  for (i in seq(1:sample_size)) {
    time = c(time,i)
    series = c(series,series[length(series)]+rnorm(1,mean = 0,sd=1))
  }
  
  data = data.frame(time = time, series = series)
  
  # Convert to timeseries data
  ts_data = ts(data = data$series, start = 0, frequency = 1)
  model = tryCatch(arima(ts_data, order = c(1,0,0),method = "ML"),error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(model)) {
    ar1 = 1-unname(model$coef[1])
    se = unname(sqrt(diag(model$var.coef))[1])
    if (!is.na(ar1) && !is.na(se)) {
      dev = c(dev,ar1/se)
    }
  }
}

# Κρίσιμη τιμή 
quantile(dev, probs = c(0.01, 0.05, 0.10))












