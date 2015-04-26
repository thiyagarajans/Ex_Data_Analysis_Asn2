#Question 3
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? 
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

#loading ggplot2, dplyr for subsetting
library(ggplot2)
library(dplyr)

# Reading RDS data, command as suggested in question page.
NEI <- readRDS("summarySCC_PM25.rds")

#Data selected for Baltimore City Alone
df1 <- subset(NEI, fips ==24510, select=c(Emissions,year,type))

#Using dplyr package, data is grouped by type & year
df2 <- group_by(df1, type, year, add= TRUE)
df3 <- summarize(df2, sum(Emissions))

#changing the column name from "sum(Emissions)" to Sum_Emissions
names(df3) = c("type", "year", "Sum_Emissions") 

#plotting begins here
png(filename = "plot3.png", width = 480, height = 480)

#ggplot to plot cumulative emissions against year
g <- ggplot(data=df3, aes(x=year, y= Sum_Emissions))

g <- g + facet_grid(.~type) + geom_bar(stat="identity", aes(fill=type)) 
g <- g + scale_x_continuous(breaks = c(1999,2002,2005,2008), labels = c("1999","2002","2005","2008")) 
g <-  g + ylab("Total Emission in Tons") + xlab("Year") 
g + geom_smooth(method="lm",color="blue", formula = y~x,size=2, se=F)

#facet_grid produces 4 panes, one for each type
#scale_x_continuos ensures proper labelling of x-axis
#geom_smooth adds the regression line to the histograms

dev.off()

#linear regression clearly shows decreasing value of Emission wrt Year for Baltimore City.
#for type "POINT" alone, there is an increase in the trend wrt to year, which 
#has reverted in 2009
