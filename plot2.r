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
png("plot2.png", width = 480, height = 480)
plot(hpct$Global_active_power ~ hpct$Datetime, type = "l",
     ylab = "Global Active Power (kilowatts)", xlab = "")
dev.off()

