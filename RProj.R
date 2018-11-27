# Import stock data

sp_data = read.csv('SP.csv')
emerging_data = read.csv('EEM.csv')

# Split data into quarters of the year

install.packages("lubridate", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(lubridate)

sp_quarters = split(sp_data, quarter(sp_data$Date, with_year=TRUE))
emerging_quarters = split(emerging_data, quarter(emerging_data$Date, with_year=TRUE))

# Analyze the coefficient of variation

calculate_cv = function(vec) {
  return (sd(vec$Adj.Close) / mean(vec$Adj.Close))
}

# Calculate cv for each quarter and combine into a single dataframe
sp_cv = lapply(sp_quarters, calculate_cv)
sp_cv = data.frame(
  Quarter = attr(sp_quarters, 'names'), 
  CV = as.data.frame(do.call("rbind", sp_cv))$V1)

emerging_cv = lapply(emerging_quarters, calculate_cv)
emerging_cv = data.frame(
  Quarter = attr(emerging_quarters, 'names'), 
  CV = as.data.frame(do.call("rbind", emerging_cv))$V1)

