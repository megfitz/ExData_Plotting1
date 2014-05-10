plot1 <- function (url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip") {
	
	##
	## Get and clean the data set
	##

	## Download and unzip the file into a folder called "data", creates the folder if it doesn't exist already
	if(!file.exists("data")){dir.create("./data")}
	if(!file.exists("./data/power/household_power_consumption.txt")){
		download.file(url,destfile="./data/power.zip")
		unzip("./data/power.zip", exdir = "./data/power")
	}

	## Loads the dataset, reads the first column in date format, and cacatenates the reformatted date data into the data set
	dataset <- read.table("./data/power/household_power_consumption.txt", sep=";", na.strings = "?", header = TRUE, colClasses = c('character', 'character', 'numeric','numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric'))
	dataset$DateTime <- strptime(paste(dataset$Date, dataset$Time), "%d/%m/%Y %H:%M:%S")

	## Filters out just the rows for the dates 2007-02-01 and 2007-02-02 and creates a new data frame with those two sets and no NA values
	set01 <- dataset[as.Date(dataset$DateTime) == "2007-02-01",]
	set02 <- dataset[as.Date(dataset$DateTime) == "2007-02-02",]
	plotting_data <- na.omit(rbind(set01,set02))

	##
	## Generate plots
	##

	##Plot 1
	png("./data/power/plot1.png", height=480, width=480)

	hist(plotting_data$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")

	dev.off()
}