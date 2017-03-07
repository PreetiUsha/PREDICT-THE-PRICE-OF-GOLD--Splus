S+ Script


tdata <-Projectdata
dim(tdata)
names(tdata)

#Histogram
par(mfcol=c(3,3))
hist(tdata$ER,main="Histogram of Exchange rate",xlab="ER",ylab="PR")
hist(tdata$Obs,main="Histogram of Obs",xlab="Obs",ylab="PR")
hist(tdata$IR,main="Histogram of Inflation rate",xlab="IR",ylab="PR")
hist(tdata$InR,main="Histogram of InterestRate",xlab="InR",ylab="PR")
hist(tdata$PD,main="Histogram of Price in dollars",xlab="PD",ylab="PR")

#Scatter Plot
par(mfcol=c(3,3))
plot(tdata$Obs,tdata$PR, main = "Price of gold in Rs  vs Obs")
plot(tdata$IR,tdata$PR, main = "Price of gold in Rs  vs Inflationrate")
plot(tdata$InR,tdata$PR, main = "Price of gold in Rs  vs Interestrate")
plot(tdata$PR,tdata$PR, main = "Price of gold in Rs  vs Price in dollars")
plot(tdata$ER,tdata$PR, main = "Price of gold in Rs  vs Exchange rate")


#Time Series plot
par(mfrow=c(3,3))
	tsplot(tdata$PR,xlab="Price ruppees vs Time"); 
	tsplot(tdata$ER,xlab="Exchange vs Time");
	tsplot(tdata$IR,xlab="Inflation rate rate vs time"); 
	tsplot(tdata$InR,xlab="Intrest rate rate vs time"); 
	tsplot(tdata$PD,xlab="Price dollar vs time"); 


smdf<-na.omit(tdata)
par(mfcol=c(3,3))
scatter.smooth (smdf$Obs,tdata$PR,main="Obs vs Price of gold in Rs",xlab="Obs",ylab="Price of gold PR")
scatter.smooth( smdf$IR,tdata$PR,main="Inflationrate vs Price of gold in Rs  ",xlab="IR",ylab="PR")
scatter.smooth (smdf$InR,tdata$PR,main="InterestRate vs Price of gold in Rs  ",xlab="InR",ylab="PR")
scatter.smooth (smdf$PD,tdata$PR,main="Price in dollars vs Price of gold in Rs  ",xlab="InR",ylab="PR")
scatter.smooth (smdf$ER,tdata$PR,main="Exchange rate vs Price of gold in Rs",xlab="ER",ylab="PR")


fdesstat<-function(inputDataFrame){
	meanVec<-apply(inputDataFrame,2,mean,na.rm=T)
	medVec<-apply(inputDataFrame,2,median,na.rm=T)
	sdVec<-apply(inputDataFrame,2,stdev,na.rm=T)
	skewVec<-apply(inputDataFrame,2,skewness,na.rm=T)
	kurtVec<-apply(inputDataFrame,2,kurtosis,na.rm=T)
	nVec<-apply(inputDataFrame,2,length)
	resultsDF<-data.frame(cbind(meanVec,medVec,sdVec,skewVec,kurtVec,nVec))
	names(resultsDF)<-c("mean","med","stdev","skew","kurt","n")
	return(resultsDF)}

smdf<-na.omit(tdata)

names(tdata)
round(fdesstat(tdata[,c("ER","Obs","IR","PD","InR","PR")]),3)
round(colMeans(tdata[,c("ER","Obs","IR","PD","InR","PR")],na.rm=T),3)
round(colStdevs(tdata[,c("ER","Obs","IR","PD","InR","PR")],na.rm=T),3)
round(cor(smdf[,c("Obs","ER","IR","PD","InR","PR")],na.method="omit"),3)

lmFit<-lm(PR~PD+Obs+IR+InR+ER,data=smdf,na.action=na.omit)
lmFit
names(lmFit)
summary(lmFit)
durbinWatson(lmFit)
preds<-predict(lmFit)
resids<-resid(lmFit)
rdf<-data.frame(smdf,p=preds,r=resids)  
names(rdf)

par(mfcol=c(1,2))
hist(rdf$r, main="Histogram of Residual",xlab="Residual")
pFit<-lm(PR~p,data=rdf)
scatter.smooth(rdf$p,rdf$PR, main="Actual vs Predicted", xlab="Predicted",ylab="Actual")

par(mfcol=c(2,2))

plot(smdf$ER,rdf$r,main="Exchange rate vs Residual",xlab="Residual",ylab="ER")

plot(smdf$Obs,rdf$r,main="Obs vs Residual",xlab="Residual",ylab="Obs")

plot(smdf$IR,rdf$r,main="Inflationrate vs Residual",xlab="Residual",ylab="IR")
plot(smdf$InR,rdf$r,main="Intrestrate vs Residual",xlab="Residual",ylab="InR")
plot(smdf$PD,rdf$r,main="IR vs Residual",xlab="Residual",ylab="PD")




#log model

lmFit<-lm(log(PR)~Obs+IR+InR+PD+ER,data=tdata,na.action=na.omit)
	summary(lmFit)
	durbinWatson(lmFit)
	names(lmFit)
	r<-lmFit$residuals
	p<-exp(lmFit$fitted.values)
	pctResid<-tdata$PR/p-1
	mean(pctResid)
	par(mfcol=c(2,2))
		hist(r);title("Histogram of Residuals")
		tsplot(r);title("Time series plot of Residuals")
		tsplot(tdata$PR,p);title("Time Series Actual Vs Predicted")
	par(mfcol=c(3,3))
		plot(tdata$Obs,r);title("Time Series Obs Vs Predicted")
		plot(tdata$InR,r); title(" Time Series Interest Rate Vs Predicted")
		plot(tdata$IR,r); title(" Time Series Inflation Rate Vs Predicted")
		plot(tdata$PD,r); title("  Time Series Price In Dollars Vs Predicted")
		plot(tdata$ER,r); title("                Time Series Exchange Rate Vs Predicted")

# Robust MM 
lmFit<-lm(log(PR)~Obs+IR+InR+PD+ER,data=tdata,na.action=na.omit)
	summary(lmFit)
	durbinWatson(lmFit)
	names(lmFit)
	r<-lmFit$residuals
	p<-exp(lmFit$fitted.values)
	pctResid<-tdata$PR/p-1
	mean(pctResid)
	par(mfcol=c(2,2))
		hist(r)
		tsplot(r)
		tsplot(tdata$PR,p)
	par(mfcol=c(2,2))
		plot(tdata$Obs,r)
		plot(tdata$PR,r)
		plot(tdata$PD,r)
		plot(tdata$Mth,r)
		plot(tdata$Yr,r)
		plot(tdata$ER,r)







# Do GAM Model
fit<-gam(PR~s(Obs)+s(IR)+s(PD)+s(InR)+s(ER),data=tdata,na.action=na.omit)
	summary(fit)
	par(mfcol=c(3,3)); plot.gam(fit)
	names(fit)
# remove overfitting....change df for each coefficient
fit<-gam(PR~s(Obs,2)+s(IR,2)+s(InR,2)+s(ER,2)+s(PD,2),data=tdata,na.action=na.omit)
	summary(fit)
par(mfcol=c(3,3));	plot.gam(fit)


