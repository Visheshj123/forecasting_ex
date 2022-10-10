

#1a

autoplot(pigs)
fc <- ses(pigs)
summary(fc)
autoplot(pigs) +
  autolayer(fc$fitted, series='fitted') +
  autolayer(forecast(fc, h=4), PI=F, series='forecasted')
#optimal values of alpha = 0.2971, l = 77260

#2) 
ses_fcast <- function(y, alpha, l){
  pred <- vector(length = length(y)+1)
  pred[1] = l
  for(i in 1:length(y)){
    pred[i+1] = alpha * y[i] + (1-alpha)*pred[i]
  }
  return(ts(pred, start=1980, frequency = 12))
}

res <- ses_fcast(pigs, 0.2971, 77260)
#from the plot we get identical values
autoplot(res) + 
  autolayer(fc$fitted, series='fitted values')


#3) 
SSE <- function(params, y){
  a <- params[1]
  l <- params[2]
  pred <- l
  err <- 0
  for(i in 1:length(y)){
    err = err + (y[i] - pred)^2
    pred = a * y[i] + (1-a)*pred
  }
  return(err)
}

optim(par=c(0.5, pigs[1]), fn=SSE, y=pigs)
#alpha = .2990081, l = 7.637927 * 10^4, slighly different than ses()

#4)
SES <- function(y, a_i, l_i){
  params <- optim(par=c(a_i, l_i), fn=SSE, y=y)$par
  res <- ses_fcast(y, params[1], params[2])
  return(res)
}

f <- SES(pigs, 0.5, pigs[1])
autoplot(fc$fitted) + 
  autolayer(f, series='manual fitted')

#5) 

#a
autoplot(books)
#there seems to be a trend with lots of noise. 

#b
ses.h <- ses(books[,'Hardcover'], h=4)
ses.p <- ses(books[,'Paperback'], h=4)
p1 <- autoplot(ses.h) + autolayer(ses.h$fitted)
p2 <- autoplot(ses.p) + autolayer(ses.p$fitted)
grid.arrange(p1,p2,nrow=2)


#c
accuracy(ses.h) #hardcover RMSE = 31.93
accuracy(ses.p) #paperback RMSE = 33.63769

holt.h <- holt(books[,'Hardcover'], h=4)
holt.p <- holt(books[,'Paperback'], h=4)
p1 <- autoplot(holt.h) + autolayer(holt.h$fitted)
p2 <- autoplot(holt.p) + autolayer(holt.p$fitted)
grid.arrange(p1,p2,nrow=2)

accuracy(holt.h) #27.193
accuracy(holt.p) #31.13692

#despite have more parameters, which lowers are denominator and therfore increases RMSE, 
#holts method still has better RMSE. This suggests better fit as well as tighter prediction intervals.

#holts method makes more sense due to the presence of a trend in the data with no seasonality. 
#holts method for paperback did marginally better, this is probably because we have a weaker trend 

#c

#The forecast for Holt-Hardcover seems better than the forecast for Holt-Paperback. 
#this is because the trend was better captured in the hardcover. The paperback seemed to have some 
#bias in the trend. 

#for ses, the forecast for hardcover make more sense given the last few data points.

#7

#normal
fc <- holt(eggs, h=100)

#dampened
fc.d <- holt(eggs, h=100, damped=T)

#boxcox
l <- BoxCox.lambda(eggs)
fc.b <- holt(eggs, lambda = l, h=100, biasadj = T)

#boxcox + dampening
fc.b.d <- holt(eggs, h=100, damped = T, lambda='auto', biasadj = T)

autoplot(eggs) + 
  autolayer(fc, series='normal forecast', PI=F) +
  autolayer(fc.d, series='dampened', PI=F) +
  autolayer(fc.b, series='Box Cox forecast', PI=F) + 
  autolayer(fc.b.d, series='dampened-BoxCox', PI=F)

#Dampening is killing the trend
#Boxcox makes it approach 0 at a much slower rate, likely the most realistic
#Boxcox + dampening makes little sense and predicts an increasing trend.

data.frame(
  normal = sqrt(mean(fc$residuals^2)),
  damped = sqrt(mean(fc.d$residuals^2)),
  boxcox = sqrt(mean((eggs - fc.b$fitted)^2)),
  boxcox.damped = sqrt(mean((eggs - fc.b.d$fitted)^2))
)

#boxcox had the lowest RMSE as well as the best forecast

#8)
retaildata <- read_excel("/Users/vishesh.javangula@ibm.com/Documents/Statistics/Time Series/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))


#a) from the autoplot alone we can see an increasing increasing seasonal magnitudes w.r.t time
#therefore seasonal multiplicity is appropriate.
autoplot(myts)

#b
fc <- hw(myts, h=1, seasonal='multiplicative')
fc.d <- hw(myts, h=1, seasonal='multiplicative', damped = T)

autoplot(myts) + 
  autolayer(fc$fitted, series='non-damped') + 
  autolayer(fc.d$fitted, series='damped')

data.frame(
  nondamped = sqrt(mean(fc$residuals^2)),
  damped = sqrt(mean(fc.d$residuals^2))
)

#undamped seem to have a better fit to the data

checkresiduals(fc)
#reject the Null that residuals are white noise

#e
myts.train = window(myts, end=c(2010,12))
myts.test = window(myts, start=2011)

fc <- hw(myts.train, h=36, seasonal = 'multiplicative')
fc.snaive <- snaive(myts.train, h=36)

accuracy(fc, myts.test)
accuracy(fc.snaive, myts.test)

#holt-winter did better on test set with RMSe of 94.8 vs 100 on snaive


#9
fc.stl.ets <- stlf(myts.train, s.window=13, robust = T,  method='ets', lambda=l, biasadj = T, h=36)
autoplot(myts.test) + 
  autolayer(fc.stl.ets, PI=F) + 
  autolayer(fc, PI=F)
accuracy(fc.stl.ets, myts.test)
#hw does the best out of all the models with a RMSe of 94.8 on the testset. 


#10
autoplot(ukcars)
#non-linear trend with seasonality. Seemed to have a sharp change in trend in 2000
m <- stl(ukcars, robust=T, s.window=5) %>% seasadj()
fcast.d <- stlf(ukcars,robust=T, s.window=5, h = 8, method='ets', etsmodel='AAN', damped=T)
fcast.h <- stlf(ukcars, robust=T, s.window=5, h = 8, method='ets', etsmodel='AAN', damped=F)
fcast.ets <- ets(ukcars, model='ZZZ')

data.frame(
  damped = sqrt(mean(fcast.d$residuals^2)),
  holt = sqrt(mean(fcast.h$residuals^2)),
  ets = sqrt(mean(fcast.ets$residuals^2))
)

#damped model had the best fit for in-sample data

#g
autoplot(window(ukcars, start=2000)) + 
  autolayer(fcast.d, series='damped forecast', PI=F) + 
  autolayer(fcast.h, series=' holt forecast', PI=F) + 
  autolayer(forecast(fcast.ets,h=8), series='ets forecast', PI=F)

#damped forecast seems the most reasonable


#h
checkresiduals(fcast.ets)
#we reject Null, so we don't yet have white noise.



#11
autoplot(visitors)
#tend with multiplicative seasonality
visitors.train <- window(visitors, end=c(2003,3))
visitors.test <- window(visitors, start=c(2003,4))
visitors.test 
visitors.train
#holt-winter multiplicative method
fcast.hw <- hw(visitors.train, seasonal = 'multiplicative', h=24)
accuracy(fcast.hw, vistors.test)
autoplot(visitors) + 
  autolayer(fcast.hw, PI=F, series='hw-multi')

#c
#multiplicative seasonality is needed here since the mangitude of the seasons seem to be
#related to the trend. magnitude is increasing as response increases. 

#d
#ets model
em <- ets(visitors.train, model='ZZM')
fcast.em <- forecast(em, h=24)

#additive ets model w/ BoxCox
l <- BoxCox.lambda(visitors.train)
ea <- ets(visitors.train, additive.only = T, lambda=l, biasadj = T)
fcast.ea <- forecast(ea, h=24, lambda=l, biasadj = T)

#seasonal naive
fcast.sn <- snaive(visitors.train, h=24)

#stl decomp model w/ BoxCox
fcast.stl <- stlf(visitors.train, h=24, method='ets', lambda=l, biasadj=T)

#e
accuracy(fcast.em, visitors.test)
accuracy(fcast.ea, visitors.test)
accuracy(fcast.sn, visitors.test)
accuracy(fcast.stl, visitors.test)
#stl with BoxCox did the best

checkresiduals(fcast.stl)
#we reject the null, therefore we do not have white noise

#f
f1 <- function(y, h){
  em <- ets(y, model='ZZM')
  return(forecast(em, h=h))
}

f2 <- function(y,h){
  l <- BoxCox.lambda(y)
  ea <- ets(y, additive.only = T, lambda=l, biasadj = T)
  fcast.ea <- forecast(ea, h=h, lambda=l, biasadj = T)
  return(fcast.ea)
}

f4 <- function(y,h){
  l <- BoxCox.lambda(y)
  return(stlf(y, h=h, method='ets', lambda=l, biasadj=T))
}

tsCV(visitors, f1, h=1)^2 %>% mean(na.rm=T) %>% sqrt() #18.38251
tsCV(visitors, f2, h=1)^2 %>% mean(na.rm=T) %>% sqrt() #18.86766
tsCV(visitors, snaive, h=1)^2 %>% mean(na.rm=T) %>% sqrt() #32.56941
tsCV(visitors, f4, h=1)^2 %>% mean(na.rm=T) %>% sqrt() #16.93

#we get the same results, with stlf w/ BoxCox out performing the others

#12
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

tsCV(qcement, fets, h=4)^2 %>% mean(na.rm=T) %>% sqrt() #0.111846
tsCV(qcement, snaive, h=4)^2 %>% mean(na.rm=T) %>% sqrt() #0.134197

#ets performed better. I expected this because of the multiplicative seasonality. Snaive
#won't handle this well. 

#13)
ausbeer.train <- subset(ausbeer, end=length(ausbeer)-12) 
ausbeer.test <- subset(ausbeer, start=length(ausbeer)-11)

bricksq.train <- subset(bricksq, end=length(bricksq)-12)
bricksq.test <- subset(bricksq, start=length(bricksq)-11)

dole.train <- subset(dole, end=length(dole)-36)
dole.test <- subset(dole, start=length(dole)-35)

a10.train <- subset(a10, end=length(a10)-36)
a10.test <- subset(a10, start=length(a10)-35)

h02.train <- subset(h02, end=length(h02)-36)
h02.test <- subset(h02, start=length(h02)-35)

usmelec.train <- subset(usmelec, end=length(usmelec)-36)
usmelec.test <- subset(usmelec, start=length(usmelec)-35)

#ausbeer model analysis 
autoplot(ausbeer)
#non-linear trend and varying seasonal magnitudes, BoxCox seems appropriate

#ausbeer ets
ets(ausbeer.train) %>% forecast(h=length(ausbeer.test)) %>% accuracy(ausbeer.test) #9.620828

#ausbeer snaive
snaive(ausbeer.train, h=length(ausbeer.test)) %>% accuracy(ausbeer.test) #10.80509

#ausbeer stlf
stlf(ausbeer.train, method='ets', lambda='auto', biasadj = T, h=length(ausbeer.test)) %>%
  accuracy(ausbeer.test) #9.893421

#ausbeer ets gave the best forecast

#bricksq
autoplot(bricksq)
ets(bricksq.train) %>% forecast(h=length(bricksq.test)) %>% accuracy(bricksq.test) #42.36696

#bricksq snaive
snaive(bricksq.train, h=length(bricksq.test)) %>% accuracy(bricksq.test) #37.15284

#bricksq stlf
stlf(bricksq.train, method='ets', lambda='auto', biasadj = T, h=length(bricksq.test)) %>%
  accuracy(bricksq.test) #941.63996

#bricksq snaive did the best


#14
autoplot(bicoal) 
#no seaonality, additive errors, high noise in data
m <- ets(bicoal)
f.ets <- forecast(m, h=15)
autoplot(bicoal) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')

#ets resorted to a mostly naive model,  while the one step foreast makes sense, it performs
#poorly on multistep forecasting

autoplot(chicken) 
#no seasonality, strong trend and relatively low noise
m <- ets(chicken)
f.ets <- forecast(m, h=15)
autoplot(chicken) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')

#provides good forecast as it doesn't make sense for time series to go below 0
#also resorted to naive model

autoplot(dole) 
#changing seasonality behavior, sharp changes in trend
m <- ets(dole)
f.ets <- forecast(m, h=35)
autoplot(dole) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')


autoplot(usdeaths) 
#weak trend, strong seasonality
m <- ets(usdeaths)
f.ets <- forecast(m, h=15)
autoplot(usdeaths) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')

autoplot(lynx) 
#seems to have cycles
m <- ets(lynx)
f.ets <- forecast(m, h=15)
autoplot(lynx) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')

summary(m)
#poor forecasts, however, this is likely because the data claimed no seasonality, however, 
#there does exist a seasonal pattern, just across years. 


autoplot(ibmclose) 
#seems to have cycles
m <- ets(ibmclose)
f.ets <- forecast(m, h=15)
autoplot(ibmclose) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')
#point forecast is fine, however much like naive, over multiple time steps it performs poorly. 

autoplot(eggs)
m <- ets(eggs)
f.ets <- forecast(m, h=15)
autoplot(eggs) + 
  autolayer(f.ets, series='ets forecast', PI=F) + 
  autolayer(m$fitted, series='fitted values')
#point forecast is fine

#overall, the presence of cycles heavily affects the forecast. This is because the model treats
#cycles as noise and therefore tries to find the level that minimizes SSE. This can result in 
#naive forecast that does not factor in cyclic behavior. 















