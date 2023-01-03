###############################
# DISCLAIMER:
# 
# The functions are distributed as is, for educational purposes only, 
# as part of the Stat485/685 course.
# 
# The author assumes no responsibility for any errors or omissions 
# and in no event shall be liable for any damages whatsoever arising 
# out of the use of the functions.
# 
# The file/functions may not be distributed in any format, 
# to anyone, without permission.
# 
###################################

#Examples Chapter 3 page 9
# Use n=10, nb_sim=1, to display one (simulated) run (nb_sim=1) of n=10 random observations (first observation is set to 0) 

#function to display the x-axis and y-axis.
setaxes=function(x0, x1, y0, y1, l = ""){
  plot(c(x0, x1), c(y0, y1), log = l, xlab = "", ylab = "", type = "n")
}
#set.seed(200555)   #set seed if wanting to reproduce your results
n=10
nb_sim=1
rho=.99    #  rho is the lag 1 correlation;  rho_k= rho^k
# set reasonable y-axis to display the series
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red, End values: blue")

for(nn in 1:nb_sim){
   series=0
  for(j in 1:n){
    series=c(series,(rho*series[j])+rnorm(1))
  }
  lines(0:n,series)  #displays the series in black
  lines(0:n,rep(mean(series),n+1),col="red")  #displays the sample average (average of n observations) in red
  #     add next line to display the last observations in blue
    points(n,series[n+1],col="blue",cex=2)  
  }

##########################################################################################################################
#Examples Chapter 3 page 10
# Use n=10, to display one (simulated) run of n=10 random observations (first observation is set to 0) 

#function to display the x-axis and y-axis.
setaxes=function(x0, x1, y0, y1, l = ""){
  plot(c(x0, x1), c(y0, y1), log = l, xlab = "", ylab = "", type = "n")
}
#set.seed(200555)   #set seed if wanting to reproduce your results
n=10
rho=.99    #  rho is the lag 1 correlation;  rho_k= rho^k

# set reasonable y-axis to display the series
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Sample average +/- SD",paste("rho=",rho)),sub="Observations: black, Sample mean: red, mean +/- SD: blue")


  series=0
  for(j in 1:n){
    series=c(series,(rho*series[j])+rnorm(1))
  }
  lines(0:n,series)  #displays the series in black
  lines(0:n,rep(mean(series),n+1),col="red")  #displays the sample average (average of n observations) in red
  # displays mean +/- one standard deviation of the n observations in green
  lines(0:n,rep(mean(series)+sd(series),n+1),col="green")
  lines(0:n,rep(mean(series)-sd(series),n+1),col="green")
  

##########################################################################################################################
#Examples chapter 3 page 22
#Linear mean function
setaxes=function(x0, x1, y0, y1, l = ""){
  plot(c(x0, x1), c(y0, y1), log = l, xlab = "", ylab = "", type = "n")
}
#set.seed(200555)  #set seed if wanting to reproduce your results
n=50    #number of observations  Try different values (20, 50, 100)
b0=10   #intercept  
b1=.1   #slope   Try steeper slopes (.5, 1, 2, 5)
rho=.2  #rho is the lag 1 correlation;  rho_k= rho^k   Try large negative/positive rho's

if (b1>0){
  setaxes(0,n,b0-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),(b0+b1*n)+2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
}else{
  setaxes(0,n,(b0+b1*n)-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),(b0)+2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
}
title(main=c("Estimated Linear Trend",paste("B0=",b0,"  B1=",b1," rho=",rho)),sub="Observations: black, Linear trend: red")

  series.lt=0   #initialize the series with a linear trend
  #Generate the stochastic component
  for(j in 1:n){
    series.lt=c(series.lt,(rho*series.lt[j])+rnorm(1))
  }
  series.lt=series.lt+b0+b1*0:n   #Add the deterministic linear trend
  lines(1:(n+1),series.lt,lwd=5)
  tseries.lt=ts(as.data.frame(series.lt))  #Create the time series objects
  model1=lm(tseries.lt~time(tseries.lt))   #Fit a linear model
  abline(model1,col="red",lwd=5)    #plot the fitted linear trend
  
  summary(model1) #Display the regression output for the fitted linear trend


##########################################################################################################################

###################
#Exercise 3.6   (U.S. monthly beer sales in millions of barrels from 1975 to 1990)
par(mfrow=c(1,1))
data(beersales)
# a) Display the time series plot for these data and interpret the plot.
win.graph(width=12, height=6,pointsize=10)
plot(beersales,ylab='Monthly Sales',type='o')

# b) Now construct a time series plot that uses separate plotting symbols for the various months. 
#   Does your interpretation change from that in part (a)?
win.graph(width=12, height=6,pointsize=10)
plot(beersales,ylab='Monthly Beer Sales',type='l')
points(y=beersales,x=time(beersales), pch=as.vector(season(beersales)))

# c) Use least squares to fit a seasonal means trend to this time series. 
#    Interpret the regression output. Save the standardized residuals from the fit for further analysis.
month.=season(beersales);beersales.lm=lm(beersales~month.);summary(beersales.lm)

# d) Construct and interpret the time series plot of the standardized residuals from part (c). 
#    Be sure to use proper plotting symbols to check on seasonality in the standardized residuals.
win.graph(width=12, height=6,pointsize=10)
plot(y=rstudent(beersales.lm),x=as.vector(time(beersales)),type='l',ylab='Standardized Residuals')
points(y=rstudent(beersales.lm),x=as.vector(time(beersales)),pch=as.vector(season(beersales)))

# e) Use least squares to fit seasonal-means plus quadratic time trend to the beer sales time series. 
#    Interpret the regression output. Save the standardized residuals from the fit for further analysis.
beersales.lm2=lm(beersales~month.+time(beersales)+I(time(beersales)^2)) 
summary(beersales.lm2)

# f) Construct and interpret the time series plot of the standardized residuals from part (e). 
#    Again use proper plotting symbols to check for any remaining seasonality in the residuals.
win.graph(width=12, height=6,pointsize=10)
plot(y=rstudent(beersales.lm2),x=as.vector(time(beersales)),type='l', ylab='Standardized Residuals')
points(y=rstudent(beersales.lm2),x=as.vector(time(beersales)),pch=as.vector(season(beersales))) 

# Histogram of std residuals 
hist(rstudent(beersales.lm2))    
hist(rstudent(beersales.lm2),breaks=12)     
     
# Q-Q plot of std residuals
qqnorm(rstudent(beersales.lm2),main='')
lines(seq(-2.5,2.5,.2),seq(-2.5,2.5,.2),type="l",col="red")

# Correlogram
acf(rstudent(beersales.lm2))  

# fitted sales for january
win.graph(width=12, height=6,pointsize=10)
plot(beersales,ylab='Monthly Sales',type='o')
points(y=beersales,x=time(beersales), pch=as.vector(season(beersales)))
summary(beersales.lm2)$coefficients
lines(as.vector(time(beersales)),-71497.79+(71.95705*as.vector(time(beersales)))-0.01810144*(as.vector(time(beersales)))^2, type="l",lwd=4,col="red")


##############################################################################################
##############################################################################################
##  EXAMPLE done in class Oct 6
# Series used in class was  (you can try other series)
#  [1]   0.00000000   0.53852427   0.41747363   0.46215088  -1.16164547  -0.49103797  -0.23086705   0.48971147
#  [9]  -1.08585608  -1.39230871   0.06011743  -1.62232428  -2.14319413  -3.59147446  -2.64975345  -4.04553984
# [17]  -4.00709874  -4.27870304  -4.88782631  -4.89572324  -5.28514015  -4.66407079  -6.04623060  -6.67024643
# [25]  -8.47909366  -9.01862060  -8.02511605  -7.52109902  -6.71506338  -7.89164417  -7.67180150  -7.17540152
# [33]  -7.64046977  -7.20437621  -6.18436722  -7.36220498  -7.85659201  -7.55044643  -8.01068529  -9.14384964
# [41] -10.96354995 -12.21482595 -12.43027140 -11.83731295 -12.98024502 -12.83703677 -12.23240912 -11.93957614
# [49] -12.64985835 -13.17831447 -11.97506168 -11.08502653 -12.72891332 -13.05504668 -10.54047795 -10.50827987
# [57] -10.61017435 -11.13330436 -10.66274299  -9.76170586  -8.16499992  -8.50526611  -8.45427845  -7.37565562
# [65]  -6.17645449  -7.07386965  -5.75789265  -5.98445684  -4.36750862  -6.06472310  -6.67710108  -7.31127975
# [73]  -8.00586891  -8.13154213  -7.21725543  -7.55983037  -6.68227159  -7.13392494  -7.69635295  -5.75878636
# [81]  -5.06611942  -4.48957020  -5.35619045  -6.13717215  -7.15741935  -9.66362034  -8.93455952  -9.23471093
# [89]  -8.80840782  -9.43374155 -11.28964020 -11.26598841 -11.07142361 -11.81015572 -12.75204474 -11.05048503
# [97]  -9.99446461  -9.35546572 -10.38168579  -9.63815277 -10.02666740

# Only need to run the next 3 lines once
setaxes=function(x0, x1, y0, y1, l = ""){
  plot(c(x0, x1), c(y0, y1), log = l, xlab = "", ylab = "", type = "n")
}

# Run the lines beginning here to simulate a "series"
#set.seed(200555)   #set seed if wanting to reproduce your results
n=100    # May or may not run for other values of n.
nb_sim=1  # Do only one simulation of n observations.
rho=.99    #  rho is the lag 1 correlation;  rho_k= rho^k

for(nn in 1:nb_sim){
  series=0
  for(j in 1:n){
    series=c(series,(rho*series[j])+rnorm(1))
  }
}
# End here to simulate the series ###############

# Now plot different cases. 
# Use first 2 lines (setaxes & title) of each case to start a new plot. 
# Use the last 2 "lines" of each case if adding to an existing plot.

# Plot all observations and the mean
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red")
lines(0:n,series,type="o")  #displays the series in black
lines(0:n,rep(mean(series),n+1),col="red",type="l")

#removing the first half of the obs.  (i.e. 50% of the observations over half the original horizon)
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red")
lines((.5*n):n,series[1+(.5*n):n],type="o")  #displays the series in black
lines((.5*n):n,rep(mean(series[1+(.5*n):n]),(.5*n)+1),col="red",type="l") 

#removing every other obs. (i.e. 50% of the observations but over the original horizon)
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red")
lines(2*0:(.5*n),series[1+2*0:(.5*n)],type="o")  #displays the series in black
lines(2*0:(.5*n),rep(mean(series[1+2*0:(.5*n)]),(.5*n)+1),col="red",type="l") 

#removing 3 out of 4 obs. (i.e. 25% of the observations but over the original horizon)
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red")
lines(4*0:(.25*n),series[1+4*0:(.25*n)],type="o")  #displays the series in black
lines(4*0:(.25*n),rep(mean(series[1+4*0:(.25*n)]),(.25*n)+1),col="red",type="l") 

#removing 9 out of 10 obs. (i.e. 10% of the observations but over the original horizon)
setaxes(0,n,-2.5*(((1-rho^(2*n))/(1-rho^2))^0.5),2.5*(((1-rho^(2*n))/(1-rho^2))^0.5))
title(main=c("Series and  Sample average",paste("rho=",rho)),sub="Observations: black, Sample means: red")
lines(10*0:(.1*n),series[1+10*0:(.1*n)],type="o")  #displays the series in black
lines(10*0:(.1*n),rep(mean(series[1+10*0:(.1*n)]),(.1*n)+1),col="red",type="l") 


###################################################################################################
###################################################################################################

# Textbook exercise 4.9
acf_ar2=function(phi1,phi2,nb_lags){
  rho=NULL
  rho[1]=phi1/(1-phi2); rho[2]=(phi2*(1-phi2)+phi1^2)/(1-phi2)
  for (k in 3:nb_lags) rho[k]=phi1*rho[k-1]+phi2*rho[k-2]
  print(rho)
  plot(1:nb_lags,rho,type="h", ylab=expression(rho[k]),xlab="k", ylim=c(-1,1)); abline(h=0)

  print(paste("Roots: ",polyroot(c(1,-phi1,-phi2))))
  if((phi1^2+4*phi2)<0){
    print(paste("Damping factor: ", sqrt(-phi2)))
    print(paste("     Frequency: ", acos(phi1/(2*sqrt(-phi2)))))
    print(paste("         Phase: ", atan((1-phi2)/(1+phi2))))
  }
}
# Run one line at a time to answer each specific case a) to f)
acf_ar2(.6,.3,10) #a
acf_ar2(-.4,.5,10) #b
acf_ar2(1.2,-.7,10) #c
acf_ar2(-1,-.6,10) #d
acf_ar2(0.5,-.9,10) #e
acf_ar2(-0.5,-.6,10) #f

###################################################################################################
###################################################################################################

############### Code for the example done in class on Nov 21,2022  ########

 data(ima22.s)   #Simulated data for IMA(2,2) illustrated in Exhibit 5.5 of the textbook
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')    # data shows a stochastic upward trend
 acf(ima22.s)     # Suggesting an MA(9) but...
 pacf(ima22.s)    # Suggesting an AR(1)  (Fitting and Diagnostics needed)
 eacf(ima22.s)    # Suggesting an ARMA(1,2) if ignoring one "X" in (1,3), or an ARMA(2,2) 
 
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
 arima(ima22.s,order=c(1,0,0),include.mean=FALSE)  # Fitting an AR(1) with mean 0, Failed
 arima(ima22.s,order=c(5,0,0))   # Fitting an AR(1) with mean mu: High corr at lag 1, mu=12
 mean(ima22.s)     # sample average of the observations:  21.5 
 res=residuals(arima(ima22.s,order=c(1,0,0)))
 plot(res)     # Residuals look random
 plot(y=res,x=zlag(res,1))
 plot(y=res,x=zlag(res,2))    # Possible positive corr at lag 2 
 acf(res)         # corr at lag 2 about 0.1 but within CI
 
 
 # Studying the first-differenced series, i.e.  ARIMA(p,1,q),  to "remove" the trend
 plot(diff(ima22.s),ylab='First Difference',type='o')   # Possible downward trend
 ima22_d1=diff(ima22.s,difference=1)   # First-differenced observations 
 acf(ima22_d1)    # Possible MA(1) or MA(3), ignoring corr at lag 9
 pacf(ima22_d1)   # Possible AR(2) or AR(3)
 eacf(ima22_d1)   # No obvious choices. Perhaps ARMA(1,2), ARMA(2,2) or ARMA(2,3)
 arima(ima22.s,order=c(2,1,0))   # Fitting ARIMA(2,1,0) to the original series, log_L=-101.2
 arima(ima22.s,order=c(1,1,2))   # Fitting ARIMA(1,1,2) to the original series, log_L=-94.8 
 res=residuals(arima(ima22.s,order=c(1,1,2)))
 plot(res)
 plot(y=res,x=zlag(res,1))
 plot(y=res,x=zlag(res,2))
 acf(res)    # Residuals for fitted ARIMA(1,1,2) look random
 arima(ima22.s,order=c(2,1,2))   # Fitting ARIMA(2,1,2) to the original series, perhaps phi_2=0 so not needed, log_L=-94.8
 # Could try other models.
 # From the models fitted, ARIMA(1,1,2) is good candidate, ARIMA(2,1,0) may also be considered
 
 
 # Studying the second-differenced series, i.e.  ARIMA(p,2,q),  to "remove" the downward trend in the first-differenced series
 plot(diff(ima22.s,difference=2),ylab='Differenced Twice',type='o')
 
 # Note: diff(ima22.s,2) is the lag 2 difference, ima22.s(t)-ima22.s(t-2), not the second order difference.
 
 ima22_d2=diff(ima22.s,difference=2) 
 acf(ima22_d2)   # Possibly MA(2)
 pacf(ima22_d2)  # Possibly AR(2)
 eacf(ima22_d2)  # Possibly ARMA(1,1) or ARMA(1,2) 
 # Could try different ARIMA(p,2,q) models.
 
 
 # Dynamics method: Trying ARMA(p,p-1) for original series, 1st and 2nd-differenced series
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
 arima(ima22.s,order=c(1,0,0))  #  Phi_1 close to 1, large response time, log_L=-117.4
 arima(ima22.s,order=c(2,0,1))  #  Phi_2>0 and close to 1. log_L=-116.2 (not much improvement for 2 additional parameters)
 arima(ima22.s,order=c(1,0,1))  #  Adding an MA term to ARIMA(1,0,0) doesn't help 
 arima(ima22.s,order=c(3,0,2))  #  Phi_3 close to 0, log_L=-97.4 
 arima(ima22.s,order=c(2,0,2))  #  Similar fit without phi_3,  (parsimony)  
 arima(ima22.s,order=c(1,1,0))  #  Trying AR(1) with first order differencing. Phi_1 close to 0 so possibly white noise 
 acf(ima22_d1)                  #  However, ACF not consistent with white noise, lag 2 and 4 corr
 arima(ima22.s,order=c(2,1,1))  #  Possible candidate, log_L=-97.8, more diagnostics needed
 arima(ima22.s,order=c(3,1,2))  #  Phi_2 and phi_3 close to 0, log_L=-94.1 
 arima(ima22.s,order=c(1,1,2))  #  Possible candidate, log_L=-94.8 
 arima(ima22.s,order=c(1,2,0))  #  Trying AR(1) with second order differencing.log_L=-103.5
 arima(ima22.s,order=c(2,2,1))  #  Phi_2 close to 0, log_L=-97.1
 arima(ima22.s,order=c(1,2,1))  #  Possible candidate, log_L=-97.9 similar to above model  
 arima(ima22_d2,order=c(2,0,1)) #  Checking fitted model using the second-differenced series. Very similar results as (2,2,1) above, as expected.
 arima(ima22.s,order=c(3,2,2))  #  Fit looks good but log_L=-94.4. Question: should we add 3 parameters? Models (1,2,1) and (1,1,2) are just as good.
 arima(ima22.s,order=c(0,2,2))  #  Trying a special case of (3,2,2) model, possible candidate, log_L=-96.1
 # Could try other special cases and should do model diagnostics
 # Preliminary analysis suggests one of ARIMA(2,0,2), (2,1,1), (1,1,2), (1,2,1), (0,2,2)
 # Interesting note: they all have p+d+q=4
 
 
 # Trying different estimation methods 
 # Looking only at the AR(1) with the original series ima22.s 
 # 
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')
 arima(ima22.s,order=c(1,0,0))  #default is CSS-ML
 arima(ima22.s,order=c(1,0,0),method="CSS-ML")
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o');abline(h=12.05)
 arima(ima22.s,order=c(1,0,0),method="ML")
 arima(ima22.s,order=c(1,0,0),method="CSS")
 plot(ima22.s,ylab="IMA(2,2) Simulation",type='o');abline(h=26.73)

 # Deciding on the parameter mu is a difficult task here. Expert knowledge would be nice.
 
  
###################################################################################################
###################################################################################################
