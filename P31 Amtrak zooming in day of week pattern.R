# P31 Amtrak zooming in day of week pattern

# ridership plot
Amtrak.data <- read.csv("Amtrak data.csv")
head(Amtrak.data)
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, ylim = c(1300, 2300), bty = "l")

# strong day of week pattern revealed by zooming in

# fit a quadratic trend P34
library(forecast)
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
par(mfrow = c(2, 1))
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
lines(ridership.lm$fitted.values, lwd = 2)

# window() extracts the subset of ts
ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim  = c(1300, 2300), bty = "l")
