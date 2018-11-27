# Import stock data

sp_data = read.csv('SP.csv')
emerging_data = read.csv('EEM.csv')

# Split data into quarters of the year

library(lubricate)

sp_quarters = split(sp_data, quarter(sp_data$Date, with_year=TRUE))
emerging_quarters = split(emerging_data, quarter(emerging_data$Date, with_year=TRUE))

# Analyze the coefficient of variation

calculate_cv = function(vec) {
  return (sd(vec$Adj.Close) / mean(vec$Adj.Close))
}

sp_cv = lapply(sp_quarters, calculate_cv)
emerging_cv = lapply(sp_quarters, calculate_cv)

