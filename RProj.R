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

# Calculate the t-test to see if emerging markets are significantly more volatile
#t.test(emerging_cv$CV, mu = mean(sp_cv$CV), alternative="greater")

t.test(emerging_cv$CV, sp_cv$CV, alternative="greater", paired=TRUE)

#t.test(emerging_cv$CV, sp_cv$CV, alternative="greater")

# mtemp = data.frame(Quarter = emerging_cv$Quarter, y1 = sp_cv$CV, y2 = emerging_cv$CV)
# mtemp = melt(mtemp)

#print(ggplot(mtemp,aes(x=mtemp$Quarter,y=value,fill=variable)) + 
#        +           geom_bar(stat="identity",position = "identity", alpha=.3))

#print(ggplot(mtemp,aes(x=mtemp$Quarter,y=value,fill=variable)) + 
#          +           geom_bar(stat="identity",position = "dodge", alpha=.3))

#ggplot(data=mtemp,aes(x=Quarter))+
#  +     geom_bar(aes(y=y2),stat="identity",position ="identity",alpha=.3,fill='lightblue',color='lightblue4') +
#  +     geom_bar(aes(y=y1),stat="identity",position ="identity",alpha=.8,fill='pink',color='red')
