plot_1 <- function() {
##Data Loading and Preprocessing        
        if(!file.exists("household_power_consumption.zip")) {
                temp <- tempfile()
                download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
                file <- unzip(temp)
                unlink(temp)
        }
        raw_data <- read.table(file, header=T, sep=";")
        unlink(file)
        raw_data$Date <- as.Date(raw_data$Date, format="%d/%m/%Y")
##Data subsetting
        sub_data <- raw_data[(raw_data$Date=="2007-02-01") | (raw_data$Date=="2007-02-02"),]
        rm(raw_data)
        sub_data$Global_active_power <- as.numeric(as.character(sub_data$Global_active_power))
        sub_data$Global_reactive_power <- as.numeric(as.character(sub_data$Global_reactive_power))
        sub_data$Voltage <- as.numeric(as.character(sub_data$Voltage))
##Transforming Date and Time
        tf_data <- transform(sub_data, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
        rm(sub_data)
        tf_data$Sub_metering_1 <- as.numeric(as.character(tf_data$Sub_metering_1))
        tf_data$Sub_metering_2 <- as.numeric(as.character(tf_data$Sub_metering_2))
        tf_data$Sub_metering_3 <- as.numeric(as.character(tf_data$Sub_metering_3))
##Plot 1
        plot1 <- function(){
                hist(tf_data$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
                dev.copy(png, file="plot1.png", width=480, height=480)
                dev.off()
                cat("Plot1.png has been saved in", getwd())
        }
        plot1()
}
plot_1()