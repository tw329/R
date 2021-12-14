###install.packages("magick")
###install.packages("imager")
###install.packages("rpart")
library("magick")
library("imager")
library("rpart")
###Set the working environment directory
setwd("C:/Users/tseng/Desktop/Rproject/imet-2020-fgvc7")
dir<-getwd()
###Read label file
trainfile<-read.csv("train.csv")
head(trainfile)
train_imageID<-as.character(trainfile[,1])
image_label<-as.character(trainfile[,2])
length(train_imageID)
trainning_sample<-as.character(sample(train_imageID,size=100))
###function to load image
load_image<-function(train_image){
		path<-paste0(dir,"/train/",train_image,".png")
		image_train<-image_read(path)
		image_resize<-image_resize(image_train,"200!x200!")
		image_transfer<-magick2cimg(image_resize)
		return (image_transfer)
}
###load all images
train_ID<-data.frame()
for (i in 1:length(trainning_sample)){
	train_ID[i,1]=as.character(trainning_sample[i])
}
img_info_storage<-apply(train_ID,1,load_image)


###load trainning table
train_table<-data.frame()
for (i in 1:length(trainning_sample)){
	train_table[i,1]=as.character(trainning_sample[i])
	train_table[i,2]=as.character(image_label[match(trainning_sample[i],train_imageID)])
}

split_table<-c()
for (i in 1:length(train_table[,2])){
	label<-as.character(train_table[i,2])
	split_label<-strsplit(label,split=" ")
	split_table<-rbind(split_table,split_label)
}



array_table<-c()
label_all<-c()
for(i in 1:length(trainning_sample)){
	for (j in 1:length(split_table[[i]])){
		detail<-c(img_info_storage[i])
		label_1<-c(split_table[[i]][j])
		array_table<-rbind(array_table,detail)
		label_all<-rbind(label_all,label_1)
		}
	}
final_table<-data.frame(array_table,label_all)
colnames(final_table)<-c("IMG","label")
#for (i in 1:length(array_table)){
	#final_table[i,1]=array_table[[i]]
	#final_table[i,2]=label_all[[i]]
#}

model<-rpart(label~.,data=final_table)