setwd("~/datasciencecoursera/exploratorydataanalysis/wk1/project")
data <- read.csv2("household_power_consumption.txt")
##--------Description--------
## Date: Date in format dd/mm/yyyy
## Time: time in format hh:mm:ss
## Global_active_power: household global minute-averaged active power (in kilowatt)
## Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
## Voltage: minute-averaged voltage (in volt)
## Global_intensity: household global minute-averaged current intensity (in ampere)
## Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
##               It corresponds to the kitchen, containing mainly a dishwasher,
##               an oven and a microwave (hot plates are not electric but gas powered).
## Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
##               It corresponds to the laundry room, containing a washing-machine,
##               a tumble-drier, a refrigerator and a light.
## Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
##               It corresponds to an electric water-heater and an air-conditioner.

##using a virable to store the position of certain dates
dates <- NA
for(i in 1:nrow(data)){
        if(data[i, "Date"] == "1/2/2007" || data[i, "Date"] == "2/2/2007"){
                if(is.na(dates[1])){
                        dates <- i
                }
                else{
                        dates <- c(dates, i)
                }
        }
}
##Subsetting data and renaming rows
data <- data[dates, ]
attributes(data)$row.names <- 1:nrow(data)
##Time format converting, while I can't figure the timezone :(
DateTime <- paste(data$Date, data$Time, sep = " ")
DateTime <- strptime(DateTime, format = "%d/%m/%Y %H:%M:%S")
DateTime <- as.POSIXlt(DateTime)
##Making the spreadsheet tidy
data <- cbind(Time = DateTime, data[, 3:ncol(data)])
for(j in 2:ncol(data)){
        data[, j] <- as.numeric(as.character(data[, j]))
}
##Opening .png device
png(filename = "plot3.png", width = 480, height = 480, units = "px",
    bg = "transparent")
##Plotting
plot(y = data$Sub_metering_1, x = data$Time, type = "n", main = NULL,
     xlab = "", ylab = "Energy sub metering", xaxt = "n")
axis(1, c(data$Time[1], data$Time[1]+24*3600, data$Time[1]+48*3600),
     c("Thu", "Fri", "Sat"))
lines(data$Time, data$Sub_metering_1, type = "l")
lines(data$Time, data$Sub_metering_2, type = "l", col = "red")
lines(data$Time, data$Sub_metering_3, type = "l", col = "blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lwd = rep(1,3), col = c("black", "red", "blue"))
##Closing .png device
dev.off()
