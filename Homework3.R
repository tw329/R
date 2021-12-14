file=('IBM', 'MSFT', 'GOOG', 'AAPL', 'AMZN', 'FB')
readfile=list()
i=1
for (f in file){
	path=paste("./Homework2_3_Data/",f,".csv",sep='')
	readfile[[i]]=read.csv(file=path,header=TRUE)
	i=i+1
}
#IBMC=readfile[[1]]["Close"]
#IBMAC=readfile[[1]]["Adj.Close"]
#MSFTC=readfile[[2]]["Close"]
#MSFTAC=readfile[[2]]["Adj.Close"]
#GOOGC=readfile[[3]]["Close"]
#GOOGAC=readfile[[3]]["Adj.Close"]
#AAPLC=readfile[[4]]["Close"]
#AAPLAC=readfile[[4]]["Adj.Close"]
#AMZNC=readfile[[5]]["Close"]
#AMZNAC=readfile[[5]]["Adj.Close"]
#FBC=readfile[[6]]["Close"]
#FBAC=readfile[[6]]["Adj.Close"]
IBM=readfile[[1]]
MSFT=readfile[[2]]
GOOG=readfile[[3]]
AAPL=readfile[[4]]
AMZN=readfile[[5]]
FB=readfile[[6]]
Divident_IBM=list()
Divident_MSFT=list()
Divident_GOOG=list()
Divident_AAPL=list()
Divident_AMZN=list()
Divident_FB=list()
divident<-function(data,result){
	k=0
	m=levels(data$Date)
	close=data["Close"]
	aclose=data["Adj.Close"]
	for (i in 2:length(close[,1])){
		close_ratio=round(close[i-1,]/close[i,],digit=5)
		adjclose_ratio=round(aclose[i-1,]/aclose[i,],digit=5)
		if (close_ratio!=adjclose_ratio){
			k=k+1
			minus=close_ratio-adjclose_ratio
			answer=minus*close[i,]
			result$date[k]=m[i]
			result$divident[k]=answer
		}
	}
	final_answer=data.frame(result)
	return (final_answer)
}

print("IBM Divident")
IBM_result<-divident(IBM,Divident_IBM)
IBM_result

print("MSFT Divident")
MSFT_result<-divident(MSFT,Divident_MSFT)
MSFT_result

print("GOOG Divident")
GOOG_result<-divident(GOOG,Divident_GOOG)
GOOG_result

print("AAPL Divident")
AAPL_result<-divident(AAPL,Divident_AAPL)
AAPL_result

print("AMZN Divident")
AMZN_result<-divident(AMZN,Divident_AMZN)
AMZN_result

print("FB Divident")
FB_result<-divident(FB,Divident_FB)
FB_result
