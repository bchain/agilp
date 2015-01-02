Loader <-function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""),t=file.path(system.file(package="agilp"),"input1","template.txt"),f="TRUE",r=2,A=2,B=5){

d<-input
template<-read.table(file = t, header = FALSE, quote = "", sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)

#select some arrays; define range of arrays within template you want to read 
#in. A is number of first array, B is last array  

arrays<-template[c(A:B),c(1:5)]
n<-dim(arrays)[1]

i<-1
j<-1
for (i in 1:n){
 
name<-paste(input,arrays[j,r], sep="")

if(file.exists(name)){
data<-read.table(file = name, row.names=1, header = TRUE, sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)

error2<-paste("Arrays number",arrays[j,r], "contains non numerical data")
if (is.numeric(data[,1])==FALSE) message(error2)

#correct for new negative
rownames(data)[which(rownames(data)=="(-)3xSLv1")]<-"3xSLv1"

if (f!="TRUE"){
if (i>1){data<-merge(data1,data,by.x="row.names",by.y="row.names")}
if(i>1){rownames(data)<-data[,1]}
data1<-data
if(i>1){data1<-data1[,-1]}
}
if (f=="TRUE"){
colnames(data)<-arrays[j,r]
outputf<-paste(output,arrays[j,r],sep = "")
write.table(data,outputf,sep="\t", col.names = NA, row.names = rownames(data))
}
#end of read loops

} else {message("Array number ",arrays[j,r], " does not exist")
arrays<-arrays[(-j),]
j<-j-1}		
#end of i loop 
j<-j+1
	}

if (f!="TRUE"){
colnames(data1)<-arrays[,r]
dataout<- NULL; rm(dataout); # Dummy to trick R CMD check
dataout<<-data1

out<-paste(output,"all_data.txt",sep="")
write.table(dataout,file=out,col.names=NA,sep="\t")
return(dataout<<-data1)
}
#END
}
