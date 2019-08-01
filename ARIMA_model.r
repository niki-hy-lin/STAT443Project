v=read.csv(file ="~/Documents/STAT443-2019/project/taxi_train.csv",header=T)
vt=read.csv(file ="~/Documents/STAT443-2019/project/taxi_test.csv",header=T)
##Taining set:365days*24hour/day=8760 (# of data)
ntot=nrow(v)
# Holdout set:
ntot_h=nrow(vt)
ntot_h
# add sines/cosines as explanatory variables, period of 24.
# WANT: get rid of seasonality
vdf=data.frame(rides=v$num_rides,x1=sin(2*pi*(1:ntot)/24),x2=cos(2*pi*(1:ntot)/24))
vdf1=data.frame(rides=vt$num_rides,x1=sin(2*pi*(1:ntot_h)/24),x2=cos(2*pi*(1:ntot_h)/24))

vtotal=c(vdf,vdf1)
vtrain=vtotal[1:ntot,]  # 8760 data in 2015(training set)
vholdo=vtotal[ntot:(ntot+ntot_h),] # 8784 data in 2016(holdout set)

fitlm=lm(rides~x1+x2,data=vtrain)
summary(fitlm)

# seems like it have increasing/decreasing trend,
# red line-fited , black line-taining set
par(mfrow=c(3,3))
matplot(1:360,cbind(vdf$rides[1:360],fitlm$fitted[1:360]),type="l",col = 1:2,
        main="hourly Jan.1-15, 2015",ylab="average taxi ride")
matplot(8400:8760,cbind(vdf$rides[8400:8760],fitlm$fitted[8400:8760]),type="l",
        col=1:2,main="hourly Dec.16-31, 2015",ylab="average taxi ride")


# residual checks
plot(fitlm$fitted,fitlm$resid,xlab="regression fitted",ylab="residual") #a few outliers
qqnorm(fitlm$resid,main="qqnorm residuals") #not follow the line especially at right side
r=ts(fitlm$resid,start=c(1,1))


plot(r,main="residuals") # a few outliers
acf(r,main="residuals") # still have seasonality, try defferenced lag ?
pacf(r,main="residuals")
plot(window(r,start=c(1,1)),main="monthly 1968 to 1997",
     ylab="resid")

# difference r because of local upward/downward trends,no lag
rdif=diff(r)
plot(window(rdif,start=c(1,1)),ylab="differenced resid",
     main="rdif")
acf(rdif,main="differenced residuals")
pacf(rdif,main="differenced residuals")

####difference r because of local upward/downward trends with lag 24*7
rdif24=diff(r,lag=24*7)
plot(window(rdif24,start=c(1,1)),ylab="differenced resid at lag24*7",
     main="rdif24*7")
acf(rdif24,main="differenced residuals at lag24*7")
pacf(rdif24,main="differenced residuals at lag24*7")

#suggests AR(3) after lag 24*7 difference with lag24*7?
dfitarima310=arima(rdif24,order=c(3,1,0),method = "CSS")
print(dfitarima310)
acf(dfitarima310$resid)
pacf(dfitarima310$resid)

#Try AR(2)
dfitarima210=arima(rdif24,order=c(2,1,0),method = "CSS")
print(dfitarima210)
acf(dfitarima210$resid)
pacf(dfitarima210$resid)

#AR(2) and AR(3) looks similar
var(rdif24) #8131629


# AR(p) after seasonal difference 
# dseas is length of cycle
arpseasdif.rmse=function(train,holdo,arvec,dseas,iprint)
{ p=length(arvec)
nholdo=length(holdo)
ntrain=length(train)
comb=c(train[(ntrain-p-dseas+1):ntrain],holdo)
fc=rep(0,nholdo)
for(i in 1:nholdo)
{ tem=comb[(i+p-1+dseas):(i+dseas)]-comb[(i+p-1):(i)]
fc[i]=comb[i+p]+sum(arvec*tem)
}
mse=mean((holdo-fc)^2)
if(iprint) print(cbind(holdo,fc))
list(rmse=sqrt(mse),forecasts=fc)
}

ar2rmse=arpseasdif.rmse(vtrain$rides,vholdo$rides,dfitarima210$coef,dseas=24*7,iprint=T)
ar3rmse=arpseasdif.rmse(vtrain$rides,vholdo$rides,dfitarima310$coef,dseas=24*7,iprint=T)

print(ar3rmse$rmse) #[1] 2868.455
print(ar2rmse$rmse) #[1] 2676.173

#Models not good maybe because of heteroscedasticity(not equal variance)