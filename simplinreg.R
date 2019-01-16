# Whooping Crane Data
# ===================
cranes <- read.csv("Data/cranes.txt",header=T)  # Reads in whooping crane data
cran <- cranes$cranes                           # Defines crane counts vector
logc <- log(cran)                               # Log of crane counts
year <- cranes$year                             # Defines years vector

plot(year,cran,pch=16,xlab="Year",ylab=    # Plot of whooping crane counts
  "Number of Cranes",cex.axis=1.5,main=    #   vs. year with axis labels,
  "Whooping Crane Population (1938-2014)", #   plotting symbol 16, and
  cex.lab=1.8,cex.main=1.8,cex=1.4,        #   control of text and point
  mgp=c(2.7,1,0))                          #   size with "cex" options

plot(year,logc,pch=16,xlab="Year",ylab=    # Plot of LOG whooping counts
  "Log(Number of Whooping Cranes)",main=   #   vs. year
  "Using a Log Transformation",cex.axis=
  1.5,cex.lab=1.8,cex.main=1.8,mgp=c(2.7,1,0))
reg <- lsfit(year,logc)            # Least squares fit of logc vs. year
abline(reg$coef,lwd=2)             # Plots the regression line with width 2

plot(year,cran,pch=16,xlab="Year",ylab=    # Plot of crane counts vs. year
  "Number of Whooping Cranes",main=
  "LOWESS & Nonlinear Fit",cex.axis=1.5,
  cex.lab=1.8,cex.main=1.8,mgp=c(2.7,1,0))
lines(lowess(year,cran,f=0.2),lwd=2,col=2, # Overlays lowess fit with f=0.2
  lty=8)
lines(lowess(year,cran),lwd=2,col=2,lty=8) # Overlays lowess fit with f=2/3
reg2 <- nls(cran ~ exp(a+b*year),start=    # Performs nonlinear least squares
  list(a=-31,b=.017))                      #   exponential model fit
coef <- summary(reg2)$coef[,1]             # Nonlinear model parameter estimates
val <- seq(min(year),max(year),.1)         # Sequence of values across years
lines(val,exp(coef[1]+coef[2]*val),lwd=2)  # Overlays the exponential fit
legend(1940,180,c("LOWESS Fits",           # Places a legend on the plot to
  "Exponential Fit"),col=c(2,1),           #   identify the lowess and least
  lty=c(8,1),lwd=c(2,2),cex=1.5)           #   squares fits

# Code for Toll Amount vs. Distance Data
# ======================================
toll <- 100*c(10,15,12,9,11,9,10,9,14,12)     # Vector of toll amounts
dist <- c(93,212,155,57,114,35,88,47,170,114) # Vector of distances traveled
plot(dist,toll,axes=F,xlab="Miles Traveled",  # Plot of toll amounts vs. distances
  ylab="Toll Amount (Lire)",xlim=c(30,215),   #   with x- and y-axis limits set
  main="Scatterplot of Toll Amount vs. Distance",
  ylim=c(800,1600),pch=16,cex=1.5,cex.lab=1.8,cex.main=1.8,mgp=c(2.7,1,0))
axis(1,at=c(30,50,100,150,200),labels=        # Controls the values appearing on the
  c("",50,100,150,200),pos=800,cex.axis=1.5)  #   x-axis (axis 1)
axis(2,pos=30,cex.axis=1.5)                   # Begins y-axis (2) at position x=30

plot(dist,toll,axes=F,xlab="Miles Traveled",  # Plot of toll amounts vs. distances
  ylab="Toll Amount (Lire)",xlim=c(30,215),
  main="Toll vs. Distance with Fitted Regression Line",ylim=
  c(800,1600),pch=16,cex=1.5,cex.lab=1.8,cex.main=1.8,mgp=c(2.7,1,0))
axis(1,at=c(30,50,100,150,200),labels=c("",   # Controls the values appearing on the
  50,100,150,200),pos=800,cex.axis=1.5)       #   x-axis (axis 1)
axis(2,pos=30,cex.axis=1.5)                   # Begins y-axis (2) at position x=30
reg.out <- lm(toll~dist)                      # LS fit of toll amount on distance
summary(reg.out)                              # Regression summary
abline(reg.out,lwd=2)                         # Plots the regression line

# Gestation vs. Longevity
# =======================
gestat <- c(219,225,122,278,31,284,201,250,   # Gestation values
  151,330,240,15,31,350)
longev <- c(18,25,5,15,6,15,8,15,8,20,12,1,   # Longevity values
  5,12)
plot(longev,gestat,pch=16,xlim=c(0,60),xlab=  # Plots gestation vs. longevity
  "Longevity (years)",ylim=c(0,800),ylab=
  "Gestation Period (days)",cex.axis=1.5,
  main="Gestation vs. Longevity",cex.lab=1.8,
  cex.main=1.8,mgp=c(2.7,1,0),cex=1.5)
reg <- lm(gestat~longev)                      # LS fit of gestation vs. longevity
summary(reg)                                  # Regression summary
abline(reg$coef,lwd=2)                        # Plots the regression line

plot(longev,gestat,pch=16,xlab="Longevity (years)",ylab=
  "Gestation Period (days)",main="Gestation vs. Longevity",
  cex.axis=1.5,cex.lab=1.8,cex.main=1.8,mgp=c(2.7,1,0),cex=1.5)
reg <- lm(gestat~longev)
abline(reg$coef,lwd=2)
anova(reg)                                    # Creates an ANOVA table of fit

