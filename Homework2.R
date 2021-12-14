IBM<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/IBM.csv",header=TRUE)
MSFT<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/MSFT.csv",header=TRUE)
GOOG<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/GOOG.csv",header=TRUE)
AAPL<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/AAPL.csv",header=TRUE)
AMZN<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/AMZN.csv",header=TRUE)
FB<- read.csv(file="C:/Users/tseng/Desktop/NJIT/Second Semster/CS 636 Data Analytics with R Program/Homework2_Data/FB.csv",header=TRUE)


IBM_ADJ<-IBM$Adj.Close
MSFT_ADJ<-MSFT$Adj.Close
GOOG_ADJ<-GOOG$Adj.Close
AAPL_ADJ<-AAPL$Adj.Close
AMZN_ADJ<-AMZN$Adj.Close
FB_ADJ<-FB$Adj.Close



corx_y<-function(x,y){
corx<-x-mean(x)
cory<-y-mean(y)
corx_sumsq<-sum(corx^2)
cory_sumsq<-sum(cory^2)
upper<-sum(corx*cory)
lower<-sqrt(corx_sumsq*cory_sumsq)
answer<-upper/lower
return (answer)
}

corx_y(IBM_ADJ,MSFT_ADJ)

stockname=c("IBM","MSFT","GOOG","AAPL","AMZN","FB")
stockdata<-data.frame(IBM_ADJ,MSFT_ADJ,GOOG_ADJ,AAPL_ADJ,AMZN_ADJ,FB_ADJ)
final_cor=list()

x=1
for (i in 1:(length(stockname)-1)){
	for (j in i:(length(stockname)-1)){
		j=j+1
		final_cor$name[x]=paste(stockname[i],":",stockname[j])
		final_cor$cor[x]= corx_y(stockdata[,i],stockdata[,j])
		x=x+1
	}
}
final_answer=data.frame(final_cor)
final_answer[order(final_answer[,2],decreasing=TRUE),]