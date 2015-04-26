#Question 5
#Across the United States, how have emissions from coal combustion-related sources
#changed from 1999-2008?

# Upload a PNG file containing your plot addressing this question.
#loading ggplot2, dplyr for subsetting
library(ggplot2)
library(dplyr)

# Reading RDS data, command as suggested in question page.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Getting SCC numbers from the column "SCC.Level.Two" that has the term "Motor" or "Vehicles)

sc1 <- subset(SCC, grepl("Motor | Vehicle", SCC.Level.Two))

#after picking SCC values, subsetting the NEI data
df1 <- subset(NEI, SCC %in% sc1$SCC, select=c(Emissions,year))

# Grouping by year, to make comparisons
df2 <- group_by(df1,year)

#divide the sum by 1000 for better readability of the plot
df3 <- summarize(df2, sum(Emissions)/1000)
names(df3) = c("year", "Total_Emissions") #changing the odd column name

#plotting begins here
png(filename="plot4.png")

#plotting all in one go
#Having points and lines, points describe the values in KTons.
ggplot(df3, aes(x=year,y=Total_Emissions)) + geom_line(color="green", size=2) + geom_point() + ylab("Emissions in kilo Tons") + scale_x_continuous(breaks = c(1999,2002,2005,2008), labels = c("1999","2002","2005","2008")) + geom_text(aes(label=round(Total_Emissions, digits=1), hjust=0.5, vjust=-0.3)) + ggtitle("Emissions from Coal Combustions")
dev.off()