plot_3 <- function() {
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
##Plot 3        
        plot3 <- function(){
                plot(tf_data$timestamp,tf_data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
                lines(tf_data$timestamp,tf_data$Sub_metering_2,col="red")
                lines(tf_data$timestamp,tf_data$Sub_metering_3,col="blue")
                legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
                dev.copy(png, file="plot3.png", width=480, height=480)
                dev.off()
                cat("plot3.png has been saved in", getwd())
        }
        plot3()
}
plot_3()