#Graph of Household power consumption for 2/1/2007-2/2/2007

get.household.power.consumption.table <- function() {
  house.power.consumption.file <- file("household_power_consumption.txt")
  house.days <- "^[1,2]/2/2007"
  #hpct is house power consumption table
  hpct <- read.table(text = grep(house.days,
                                 readLines(house.power.consumption.file), value = TRUE),
                     col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", 
                                   "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                     colClasses = c('character', 'character', 'numeric',
                                    'numeric', 'numeric', 'numeric',
                                    'numeric', 'numeric', 'numeric'),
                     sep = ";", header = TRUE)
  
  
  hpct$Date <- as.Date(hpct$Date, format="%d/%m/%Y")
  hpct$Datetime <- as.POSIXct(paste(as.Date(hpct$Date), hpct$Time))
  hpct
}

if(!exists("hpct"))
{
  hpct <- get.household.power.consumption.table()
}

png("plot4.png", width = 480, height = 480)

par(mfrow=c(2,2))
with(hpct, {
  plot(Global_active_power ~ Datetime, type = "l", 
       ylab = "Global Active Power", xlab = "")
  plot(Voltage ~ Datetime, type = "l", ylab = "Voltage", xlab = "datetime")
  plot(Sub_metering_1 ~ Datetime, type = "l", ylab = "Energy sub metering",
       xlab = "")
  lines(Sub_metering_2 ~ Datetime, col = 'Red')
  lines(Sub_metering_3 ~ Datetime, col = 'Blue')
  legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
         bty = "n",
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power ~ Datetime, type = "l", 
       ylab = "Global_reactive_power", xlab = "datetime")
})
dev.off()

