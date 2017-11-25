# data-clean.R script created for WineConsumption project

setwd("/Users/alex/Documents/datasets/wine")

# Wine Consumption data
df <- read.csv("data/WineConsumptioninUS.csv", header=TRUE)

# remove text "gals" and convert
df[,2] <- as.numeric(gsub(" gals","",df[,2]))

# remove text "million" and convert
df[,3:4] <- apply(df[,3:4],2, function(x) as.numeric(gsub(" million","",x)))

# convert year column to dates
df[,1] <- format(as.Date(as.character(df$Year),"%Y"),"%Y")

# convert columns from gallons into litre (1 US gallon to 3.78541 litres)
df[,c("Total.Wine.per.Resident.litres",
      "Total.Wine.litres","Total.Table.Wine.litres")] <- df[,2:4]*3.78541


# US Population Total
df.pop <- read.csv("data/API_SP.POP.TOTL_DS2_en_csv_v2.csv", skip = 4)
df.pop.us <- fnDfTransform.USA(df.pop, "US.Population.Total")

# US per capita
df.gdp <- read.csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv", skip = 4)
df.gdp.us <- fnDfTransform.USA(df.gdp,"GDP.per.capita.USD")

