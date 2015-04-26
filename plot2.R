#Question 2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.


# Reading RDS data, command as suggested in question page.
#using dplyr for Grouping & Summary
library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

#seperate the data yearwise into 4 different data frames. 
#Data selected for Baltimore City Alone
df1 <- subset(NEI, fips ==24510, select=c(Emissions,year))
df2 <- group_by(df1,year)
df3 <- summarize(df2,sum(Emissions))

names(df3) = c( "year", "Sum_Emissions") 


#plotting begins here
png(filename = "plot2.png")
plot(df3$year,y=df3$Sum_Emissions, type="h", lwd=10, main = "Yearwise total Emissions in Baltimore", xlab='Year', ylab="Total Emission in Tons", xaxt='n')

#linear regression clearly shows decreasing value of Emission wrt Year for Baltimore City
fit <- lm(df3$Sum_Emissions~df3$year)
abline(fit,col="blue",lwd=2)

#decorating axes appropriately overriding default limits

axis(1, at = c(1999, 2002, 2005,2008), labels = c("1999", "2002", "2005","2008"))
dev.off()