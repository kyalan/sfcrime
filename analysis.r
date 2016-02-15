setwd("C:/Users/KY/Documents/GitHub/sfcrime")
d.train<-read.csv("train.csv", stringsAsFactors=F)
d.test<-read.csv("test.csv", stringsAsFactors=F)
cat("raw data loaded.\n")

DataProcessing<-function(d) 
{
	library(lubridate)
	d$Dates<-fast_strptime(d$Dates, format="%Y-%m-%d %H:%M:%S", tz="UTC") 
	d$Day<-day(d$Dates) 
	d$Month<-month(d$Dates) 
	d$Year<-year(d$Dates) 
	d$Hour<-hour(d$Dates) 
	d$Minute<-minute(d$Dates) 
	d$Second<-second(d$Dates)

	library(plyr)
	d$Intersection<-grepl("/", d$Address) 
	d$Intersection<-plyr::mapvalues(d$Intersection,from=c("TRUE","FALSE"),to=c(1,0))

	d$Night<-ifelse(d$Hour > 22 | d$Hour < 6,1,0)
	d$Week<-ifelse(d$DayOfWeek=="Saturday" | d$DayOfWeek=="Sunday",0,1)
	return(d)
}

d.train<-DataProcessing(d.train)
d.test<-DataProcessing(d.test)

getcatMatrix<-function(d)
{
	categoryMatrix<-data.frame(with(d,model.matrix(~Category+0))) 
	names(categoryMatrix)<-sort(unique(d$Category)) 
	d<-cbind(categoryMatrix,d)
	return(d)
}

d.train<-getcatMatrix(d.train)

# d.train.tr.index<-sample(1:nrow(d.train),0.7*nrow(d.train)) 
# d.train.tr<-d.train[d.train.tr.index,] 
# d.train.test<-d.train[-d.train.tr.index,]
d.train.tr<-d.train
d.train.test<-d.test

library(glmnet)
matMod.tr<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+Hour+Minute+Intersection+Night,data=d.train.tr) 
matMod.test<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+Hour+Minute+Intersection+Night,data=d.train.test)

m<-glmnet(matMod.tr,d.train.tr[,1],family="binomial")
pred<-as.data.frame(predict(m,matMod.test,s=1e-15,type="response"))
numCat<-length(unique(d.train.tr$Category))
write.csv(pred, paste0(1, ".csv"))
pb <- txtProgressBar(min = 1, max = numCat, style = 3)
for (i in 22:numCat) 
{
	m<-glmnet(matMod.tr,d.train.tr[,i],family="binomial")
	# pred<-cbind(pred,predict(m,matMod.test,s=1e-15,type="response"))
	pred<-predict(m,matMod.test,s=1e-15,type="response")
	write.csv(pred, paste0(i, ".csv"))
	setTxtProgressBar(pb, i)
}

