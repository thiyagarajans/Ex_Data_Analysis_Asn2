#Question 1 
#Have total emissions from PM2.5 decreased in the United States 
#from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources 
#for each of the years 1999, 2002, 2005, and 2008.

# Reading RDS data, command as suggested in question page.
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

#seperate the data yearwise into 4 different data frames.
df1 <- subset(NEI,  select=c(Emissions,year))
df2 <- group_by(df1,year)
df3 <- summarize(df2,sum(Emissions))

names(df3) = c("year","Emissions")

#plotting begins here
png(filename = "plot1.png")
plot(x=df3$year,y=df3$Emissions/1000, type="h", lwd=10, main = "Yearwise Total Emissions", xlab='Year', ylab="Total Emission in kTons", xaxt='n')
# divide by 1000 ensures kiloton units

#linear regression clearly shows decreasing value of Emission wrt Year
fit <- lm(df3$Emissions/1000~df3$year)
abline(fit,col="blue",lwd=2)

#decorating axes appropriately overriding default limits

axis(1, at = c(1999, 2002, 2005,2008), labels = c("1999", "2002", "2005","2008"))
dev.off()