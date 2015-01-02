Baseline <-
function(NORM="LOG",allfiles="TRUE",r=2,A=2,B=3,input=file.path(system.file(package="agilp"),"input",""),baseout=file.path(system.file(package="agilp"),"output","baseline.txt"),t=file.path(system.file(package="agilp"),"input1","template.txt")){

if (allfiles != "TRUE"){template<-read.table(file = t, quote = "",comment.char = "", sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
arrays<-template[c(A:B),r]}

if(allfiles=="TRUE"){arrays<-dir(input)}
n<-length(arrays)

#load first array 
d<-input 
name<-paste(d,arrays[1], sep="")

if(file.exists(name)){
data1<-read.table(file = name,row.names=1,header=TRUE,comment.char = "", sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)

if (is.numeric(data1[,1])==FALSE) message("non-numeric data")

m<-dim(data1)[1]


# load arrays
if (NORM=="LOG") {data<-data1
data[,1]<-log2(data[,1])} else data<-data1
i<-2

for (i in 2:n){ 
name<-paste(d,arrays[i], sep="")
error<-paste("Arrays number",arrays[i], "is not present")
if(file.exists(name)){
datai<-read.table(file = name,row.names=1,header=TRUE, comment.char = "", sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
if (is.numeric(datai[,1])==FALSE) print (error)
data<-merge(data,datai,by.x="row.names",by.y="row.names")
if (NORM=="LOG") { data[,3]<-log2(data[,3])}

data[,2]<-data[,2]+data[,3]
editprobes<-data[,1]
data<-data[,-1]
data<-as.data.frame(data[,-2])
row.names(data)<-editprobes
}
else message("Array number ",arrays[i], " is not present")
}
mean<-data
mean[,1]<-data[,1]/i


write.table(mean,baseout,col.names=NA,sep="\t")

}
else message("the first file is missing so the programme stops")


}
