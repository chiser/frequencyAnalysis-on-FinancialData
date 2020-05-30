# load quantmod, then some data, and charts away...
library(quantmod)

getSymbols(c('QQQ','SPY',"IBM","AAPL"))

# barChart(IBM)
# candleChart(IBM)
# lineChart(IBM)

aktie_ts<- SPY$SPY.Close #QQQ$QQQ.Close #AAPL$AAPL.Close  #IBM$IBM.Close

data<-as.data.frame(aktie_ts)
# time<-index(data)
time<-1:nrow(data[1])


### Start with EMD

library (EMD)
library (hht)

ee <- EEMD (data[,1],time,250, 10, 10, "trials")
eec <- EEMDCompile ("trials", 10, 10)

plot (eec$tt, eec$averaged.imfs[,1], type="l")
lines (eec$averaged.imfs[,2], col="blue")
lines (eec$averaged.imfs[,3], col="grey")
lines (eec$averaged.imfs[,4], col="green")
lines (eec$averaged.imfs[,5], col="orange")
lines (eec$averaged.imfs[,6], col="red")
lines (eec$averaged.imfs[,7], col="yellow")
lines (eec$averaged.imfs[,8], col="darkblue")
lines (eec$averaged.imfs[,9], col="darkgreen")
lines (eec$averaged.imfs[,10], col="darkred")

# Let’s do the same thing with SSA:
library (Rssa)
# Decompose series with default parameters
s <- ssa(data[,1]) #, kind = "mssa"
# Show the summary
summary(s)
# Reconstruct the series, with suitable grouping
r <- reconstruct(s) #groups = list(Trend = 1:2) , groups = list(Trend= c(1, 2), c(3, 4), c(5, 6))
plot(r)

# Plot original series, trend and residuals superimposed
plot(r, plot.method = "xyplot", superpose = TRUE,
     auto.key = list(columns = 3),
     col = c("blue", "green", "red", "violet"),
     lty = c(rep(1, 4), rep(2, 4), rep(3, 4)))

# FFT
# fftData<-fft(data[,1])
# 
# plot(fftData)

## Check the spectrum
library(seewave)

# op<-par(mfrow=c(2,2))
# spec(data[,1],f=22050)
# spec(data[,1],f=22050,col="red",plot=2)
spec(data[,1],f=22050,dB="max0",col="blue")
# spec(data[,1],f=22050,dB="max0",col="green",plot=2)
# par(op)

## Spectral
library(lattice)
library(spectral)

spectrum(data[,1])
