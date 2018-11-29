# Import stock data

sp_data = read.csv('SP.csv')
emerging_data = read.csv('EEM.csv')

# Split data into quarters of the year

#install.packages("lubridate", dependencies=TRUE, repos='http://cran.rstudio.com/')
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

# Calculate the t-test to see if emerging markets are significantly more volatile

t.test(emerging_cv$CV, sp_cv$CV, alternative="greater")

# Graph the data

library(ggplot2)

g_data = data.frame(Quarter = emerging_cv$Quarter, SP = sp_cv$CV, Emerging = emerging_cv$CV)

ggplot(data=g_data,aes(x=Quarter, y=CV))+scale_x_discrete(limits = rev(levels(g_data$Quarter)))+geom_bar(aes(y=Emerging),stat="identity",position ="identity",fill='lightblue',color='black') +geom_bar(aes(y=SP),stat="identity",position ="identity",fill='pink',color='black')+geom_bar(aes(y=Emerging),stat="identity",position ="identity",alpha=0,color='black') +coord_flip() + scale_y_continuous(expand = c(0, 0), limits = c(0, 0.13)) +xlab("Year and Quarter") + ylab("Coefficient of Variation (Unitless)")+ggtitle("Coefficient of Variation for Different Market Quarters", subtitle="Note: The Bars are Overlaid")
