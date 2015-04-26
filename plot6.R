#Question 6
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#loading ggplot2, dplyr for subsetting
library(ggplot2)
library(dplyr)

# Reading RDS data, command as suggested in question page.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Getting SCC numbers from the column "SCC.Level.Two" that has the term "Motor" or "Vehicles)

sc1 <- subset(SCC, grepl("Motor | Vehicle", SCC.Level.Two), select=SCC)

#after picking SCC values, subsetting the NEI data
df1$fips[df1$fips=="24510"] <- "Baltimore"
df1$fips[df1$fips=="06037"] <- "Los Angelis"

# Grouping by year, to make comparisons
df2 <- group_by(df1, fips,year, add=TRUE)

df3 <- summarize(df2, sum(Emissions))
names(df3) = c("fips", "year", "Total_Emissions") #changing the odd column name

#plotting begins here
png(filename="plot6.png", width=480, height=560)

#plotting all in one go
#Having Histograms of year-wise total emissions in two panels.
ggplot(df3, aes(x=year,y=Total_Emissions, fill=fips)) + geom_bar(stat="identity") + geom_point() + facet_grid(.~fips) + ylab("PM2.5 Emissions in Tons") + scale_x_continuous(breaks = c(1999,2002,2005,2008), labels = c("1999","2002","2005","2008")) + geom_text(aes(label=round(Total_Emissions, digits=1), hjust=0.5, vjust=-0.5)) + ggtitle("Motor Vehicle Emissions Baltimore vs Los Angelis")
dev.off()