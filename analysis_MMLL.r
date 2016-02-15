setwd("C:/Users/KY/Documents/GitHub/sfcrime")

act<-as.data.frame(d.train.tr[1:numCat])
pred<-c()
for (i in 1:numCat)
{
	d<-read.csv(paste0(i, ".csv"), header=T)
	names(d)<-c("Id", names(d.train.tr)[i])
	if (is.null(dim(pred))) {
		pred<-d
	} else {
		pred<-merge(pred,d)
	}
}
rownames(pred)<-pred$Id
pred<-pred[,-1]
Id<-0:(nrow(pred)-1)
pred<-cbind(Id, pred)

MMLL <- function(act, pred, eps=1e-15) 
{ 
	pred[pred < eps] <- eps 
	pred[pred > 1 - eps] <- 1 - eps -1/nrow(act)*(sum(act*log(pred))) 
}
print(MMLL(act,pred))

write.csv(pred, "solution.csv", row.names=F)