# load quantmod, then some data, and charts away...
library(quantmod)
getSymbols("IBM")

barChart(IBM)
candleChart(IBM)
lineChart(IBM)

data<-as.data.frame(IBM$IBM.Close)
time<-index(data)
time<-1:length(data)
# index(A[diff(A)<0])

### Start with EMD

library (EMD)
library (hht)

ee <- EEMD (data$IBM.Close,time,250, 10, 6, "trials")
eec <- EEMDCompile ("trials", 10, 6)

plot (eec$tt, eec$averaged.imfs[,6], type="l")
lines (eec$averaged.imfs[,1], col="red")
lines (eec$averaged.imfs[,2], col="blue")
lines (eec$averaged.imfs[,3], col="grey")
lines (eec$averaged.imfs[,4], col="green")
lines (eec$averaged.imfs[,5], col="orange")

# Let’s do the same thing with SSA:
library (Rssa)
# Decompose series with default parameters
s <- ssa(data$IBM.Close) #, kind = "mssa"
# Show the summary
summary(s)
# Reconstruct the series, with suitable grouping
r <- reconstruct(s, groups = list(c(1, 4), c(2, 3), c(5, 6))) #groups = list(Trend = 1:2)
plot(r)

# Plot original series, trend and residuals superimposed
plot(r, plot.method = "xyplot", superpose = TRUE,
     auto.key = list(columns = 3),
     col = c("blue", "green", "red", "violet"),
     lty = c(rep(1, 4), rep(2, 4), rep(3, 4)))

# lines (gas.rec$T, col="red")

# FFT

fftData<-fft(data$IBM.Close)

plot(fftData)


## Check the spectrum
library(seewave)

op<-par(mfrow=c(2,2))
spec(data$IBM.Close,f=22050)
spec(data$IBM.Close,f=22050,col="red",plot=2)
spec(data$IBM.Close,f=22050,dB="max0",col="blue")
spec(data$IBM.Close,f=22050,dB="max0",col="green",plot=2)
par(op)
