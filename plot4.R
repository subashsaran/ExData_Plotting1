plot_4 <- function() {
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
##Plot 4
        plot4 <- function(){
                par(mfrow=c(2,2))
                
                ##PLOT 4.1
                plot(tf_data$timestamp,tf_data$Global_active_power, type="l", xlab="", ylab="Global Active Power")
                ##PLOT 4.2
                plot(tf_data$timestamp,tf_data$Voltage, type="l", xlab="datetime", ylab="Voltage")
                
                ##PLOT 4.3
                plot(tf_data$timestamp,tf_data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
                lines(tf_data$timestamp,tf_data$Sub_metering_2,col="red")
                lines(tf_data$timestamp,tf_data$Sub_metering_3,col="blue")
                legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
                
                #PLOT 4.4
                plot(tf_data$timestamp,tf_data$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
                
                #OUTPUT
                dev.copy(png, file="plot4.png", width=480, height=480)
                dev.off()
                cat("plot4.png has been saved in", getwd())
        }
        plot4()
}
plot_4()