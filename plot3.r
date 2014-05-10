plot3 <- function (url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip") {
	
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

	##Plot 3
	png("./data/power/plot3.png", height=480, width=480)

	plot(plotting_data$DateTime,plotting_data$Sub_metering_1, type="l", xlab = "", ylab = "Energy sub metering")
	lines(plotting_data$DateTime,plotting_data$Sub_metering_2, col = "red")
	lines(plotting_data$DateTime,plotting_data$Sub_metering_3, col = "blue")
	legend(x = c("topright"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1), lwd=c(2.5,2.5),col=c("black", "red", "blue"))

	dev.off()
}