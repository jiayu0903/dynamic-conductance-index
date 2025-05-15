library(ggplot2)
library(cowplot)
library(readxl)
library(simET)

# Directory of the data
dr <- "./data/"
date <- "2024-04-17"
time <- "10:42:46"
t0 <- as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%OS")

# Read and process Leaf temperature data
dL <- read.csv(paste0(dr, "LeafT.csv"))
names(dL) <- c("Date", "Time",	"Reflect","Black","Wet") 

# Read and process environment data
dE <- read.csv(paste0(dr, "Envir.csv"))
dE$time <- as.POSIXct(paste(date, dE$Time), format="%Y-%m-%d %I:%M:%OS %p")
ggplot(dE, aes(x=time, y=Temperature)) + geom_point()
ggplot(dE, aes(x=time, y=Humidity)) + geom_point()

dE2 <- read.csv(paste0(dr, "Envir-wet.csv"))
dE2$time <- as.POSIXct((dE2$Date.Time),  format="%Y-%m-%dT%H:%M:%OS")

#Convert to numeric data
dE$time <- as.numeric(dE$time - t0)
dE2$time <- as.numeric(dE2$time - t0)
dL$time <- as.numeric(dL$Time)

# Use splines to extract data at common time points
spl_A <- splinefun(dE$time, dE$Temperature)
spl_R <- splinefun(dE$time, dE$Humidity)
spl_AW <- splinefun(dE2$time, dE2$Tair.wet)
spl_RW <- splinefun(dE2$time, dE2$RH.wet)

fitt <- smooth.spline(dE$time, dE$Temperature, cv=TRUE)
fitt2 <- smooth.spline(dE$time, dE$Humidity, cv=TRUE)
fitt3 <- smooth.spline(dE2$time, dE2$Tair.wet, cv=TRUE)
fitt4 <- smooth.spline(dE2$time, dE2$RH.wet, cv=TRUE)

fitt$df
fitt2$df
fitt3$df
fitt4$df

# Remove high frequency fluctuations
sspl_A <- smooth.spline(dE$time, dE$Temperature, df=10)
sspl_R <- smooth.spline(dE$time, dE$Humidity, df=15)
sspl_AW <- smooth.spline(dE2$time, dE2$Tair.wet, df=137)
sspl_RW <- smooth.spline(dE2$time, dE2$RH.wet, df=134)

time <- dL$time 

# Merge the data sets
d <- data.frame(time=dL$time, Tair=predict(sspl_A, time)$y,
                Tair.wet=predict(sspl_AW, time)$y,
                RH=predict(sspl_R, time)$y,
                RH.wet=predict(sspl_RW, time)$y)

# Dry&Wet reference temperature and environment data (adjust by real environment data)
Twet <- dL$Wet
Tdry <- dL$Black
Treflect <- dL$Reflect
Tair <-d$Tair.wet
RH <- (d$RH.wet/100)

#plot test
#plot(x=time, y= Twet)

# Known parameters
Pa <- 101300
R <- 8.3145
E <- 3.12637E-05
 # kg m-2 s-1
 

#VPD
ea <- 613.65 * exp(17.502 * Tair / (240.97 + Tair)) * RH
es <- 613.65 * exp(17.502 * Twet / (240.97 + Twet))

SH <- 0.622 * ea / (Pa - ea)
Cs <- 1005 + 1820 * SH
rho <- Pa / (287.058 * (Tair+273))
lambda <- 1.91846E6 * ((Twet+273) / (Twet+273 - 33.91))**2


# steady-status
emissBT <- 0.97 # emissivity of the black tape
emissR <- 0.95 # emissivity of the filter paper

# steady-status

gbh_numerator_steady <- Tdry-Twet -(emissBT *5.6703E-8*((Twet+273)**4-(Tdry+273)**4)+emissR *5.6703E-8*((Twet+273)**4-(Tdry+273)**4))

gbh_denominator_steady <- 2*rho*Cs*(Twet-Tdry)+lambda*0.018*(es-ea)/(0.92*R*(Twet+273))

gbh <- gbh_numerator_steady/gbh_denominator_steady # unit  m s-1

gbw <- (2*Pa/(0.92*R*(Twet+273))) * gbh # mol m-2 s-1 one sided

# gsw from transpiration rate

gtw <- E / (0.018*((es-ea)/Pa)) 
gsw <- (gtw*gbw) / (gbw-gtw)  

#plot(x=time,y=Twet)
mean(gbw)
mean(gbh)


#plot(x=time,y=gsw)


