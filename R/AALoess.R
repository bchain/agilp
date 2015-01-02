AALoess <-
function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""), baseline=file.path(system.file(package="agilp"),"input1","testbase.txt"),LOG="TRUE"){
#input baseline

c3<-read.table(file = baseline, row.names=1, header=TRUE, sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
#c2<-as.numeric(c3[,1])

arrays<-dir(input)
n<-length(arrays)
##############################################################################
#set up array with which to collect the sum of squared errors between 
#baseline mean and predicted 
SSE1<- arrays
dim(SSE1)<-c(n,1)
zeros<-rep(0,n)
dim(zeros)<-c(n,1)
SSE<-cbind(SSE1,zeros)

#############################################################################
#
#read in raw datafiles

i<-1
for (i in 1:n){
#load in raw unnormalised data
name<-file.path(input,arrays[i], fsep = .Platform$file.sep)

if(file.exists(name)){
data2<-read.table(file = name,header=TRUE,row.names=1,sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
data3<-merge(c3,data2,by.x="row.names",by.y="row.names")
c1<-as.numeric(data3[,3])
c2<-as.numeric(data3[,2])

###############################################################################

if (is.numeric(c1)==FALSE) message("non-numeric data")
if (LOG=="TRUE"){c1<-log(c1,2)}
loessdata<-loess(c1 ~ c2, span = 0.1)

#calculate the new normalised data; z is difference between predicted and actual

zg<-c2-predict(loessdata,c2)
zgsq<-zg*zg
SSE[i,2]<-sum(zgsq)
norm<-data.frame(c1+zg)
rownames(norm)<-data3[,1]

##############################################################################
#Output
#write file with normalised data

array_name<-if(grepl("Raw",arrays[i],ignore.case=TRUE)){sub("Raw","",arrays[i],ignore.case=TRUE)}else {
array_name<-arrays[i]}
dg<-paste(output,"n",array_name,sep = "")
colnames(norm)<-paste("n",array_name,sep="")
write.table(norm,dg,sep="\t",col.names=NA,row.names=TRUE)
}else message("Array ",arrays[i], "is not present")

#end of for loop for each file
}
#write file with squared sum errors
colnames(SSE)<-c("array","SSE")
dg1<-paste(output,"SSE_",Sys.Date(),".txt",sep = "")
write.table(SSE,dg1,sep="\t",col.names=TRUE,row.names=FALSE)
hist(as.numeric(SSE[,2]))

#end of function

}
