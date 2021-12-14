print("1.20")
data(islands)
head(islands)
new_islands<-sort(islands,decreasing=TRUE)
new_islands[new_islands>10000]

print("1.21")

install.packages("UsingR")
library("UsingR"); ###data(primes,package="UsingR")
head(primes)

print("1.21(1)")

len_total<-length(primes)
print(paste("There are",len_total))

print("1.21(2)")

len_100<-length(primes[primes<100])
print(paste("There are",len_100))

print("1.21(3)")
len_1000<-length(primes[primes>100&primes<1000]) 
print(paste("There are",len_1000 ))

print("1.22")

print("1.22(1)")

primes[-1]
print("primes[-1] return every elements in primes except first one.")

print("1.22(2)")

print("Since the n is set to be the length of the primes data, the index [-n] return all the elements but nth one, which means that primes[-n] will return all elements except last one.")

print("1.22(3)")

range(primes)
cat("Because primes[-1] has no element 2, and primes[-n] has no element 2003, which means that the first element of primes[-1] is the second element of primes[-n] \n so we can check the twin primes by adding two, and see if it still exists in primes")

count=0
for (n in primes){
k=n+2
if ( k %in% primes)count=count+1
}
count
print(paste("There are", count, "twin primes"))


print("1.23")

data_summary<-function(summary_data){
observation<-length(summary_data)
min_observation<-min(summary_data)
max_observation<-max(summary_data)
creteria_15<-length(summary_data[which(summary_data>1.5)])
result<-list(observation, min_observation, max_observation, creteria_15)
return (result)
}
print(data_summary(treering))


print("1.24")

print("1.24(1)")

count=0
r_name=row.names(mandms)
for (n in mandms){
count=count+1
counts=0
for (k in n){
counts=counts+1
if (k == 0) print (r_name[counts])
}
}

print("1.24(2)")

TF_mandms<-mandms==rowMeans(mandms)
row.names(mandms[rowSums(TF_mandms)==6,])

print("1.24(3)")

max_TF<-mandms==max(mandms)
row.names(mandms[rowSums(max_TF)==1,])
names(mandms[colSums(max_TF)==1])

print("1.25")

print("1.25(1)")

head(nym.2002)
length(nym.2002[,1])

print("1.25(2)")

f_time<-min(nym.2002$time)
f_time

convert_to_minute<-function(seconds){
min<-seconds%/%60
sec<-round(seconds%%60)
print(paste("Equals to",min,":",sec,"minutes"))
}
convert_to_minute(f_time)

print("1.25(3)")

s_time<-max(nym.2002$time)
convert_to_minute(s_time)

print("1.26")

print("1.26(1)")

data(rivers)
head(rivers)
l_river<-max(rivers)
index_longest<-match(l_river,rivers)
print(paste("The longest river is ",index_longest,"th river", ":", l_river  ))

print("1.26(2)")

s_river<-min(rivers)
index_shortest<-match(s_river, rivers)
print(paste("The shortest river is",index_shortest,"th river",":", s_river ))

print("1.27")

print("1.27(1)")

data(uspop)
head(uspop)
names(uspop)<-seq(1790,1970,by=10)
head(uspop)

print("1.27(2)")

print(diff(uspop))
largest_diff<-match(max(diff(uspop)),diff(uspop))
largest_diff
uspop[c(largest_diff,largest_diff+1)]

print("1.27(3)")

print(diff(uspop)>0)
cat("because the us people life are getting better and better, thus they have more intention to enjoy their lifes, \n and from 1900, with the invention of Radio/Television, people have more access to know about the pop culture \n as the result shows, every difference between decades are positive, so yes this is the case with the data")
