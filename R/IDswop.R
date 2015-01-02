IDswop <-
function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""),fun="mean",annotation=file.path(system.file(package="agilp"),"input1","annotation.txt"), source_ID = "ProbeID", target_ID="EnsemblID", ERR=0.2){

#set of files to be processed
datafiles<-dir(input)

#input annotation file
annotation1<-read.table(annotation,header = TRUE, fill=TRUE,quote="", sep = "\t",   stringsAsFactors=FALSE )

#input data files
i<-1

if (ERR>=0 & ERR<=1){
for (i in 1:length(datafiles)){
name<-paste(input,datafiles[i],sep="")
error<-paste("Arrays number",datafiles[i], "is not present")
if(file.exists(name)){
datar<-read.table(name,header=TRUE,row.names=1,fill=TRUE,sep = "\t",  stringsAsFactors=FALSE)

#merge data with its own annotation
IDs<-merge(annotation1,datar,by.x=source_ID,by.y="row.names")

col1<-levels(as.factor(IDs[,target_ID]))
if(fun=="mean"){col2<-as.vector(tapply(IDs[,dim(IDs)[2]],factor(IDs[,target_ID]),mean))}
if(fun=="max") {col2<-as.vector(tapply(IDs[,dim(IDs)[2]],factor(IDs[,target_ID]),max))}

col3<-as.vector(tapply(IDs[,dim(IDs)[2]],factor(IDs[,target_ID]),sd))
#col4<-tabulate(factor(IDs[,target_ID]))
col4<-col3/col2
newID<-data.frame(col1,col2,col3,col4)

#filter out all rows where the duplicates differ by more than ERR
newID1<-newID[(is.na(newID[,4])==T | newID[,4]<ERR),]

#remove any rows that have no identifier 
newID2<-newID1[newID1[,1]!="",]
newID3<-data.frame(newID2[,2])

name1<-paste(output,sub(".txt","",datafiles[i],),"_",target_ID,".txt",sep="")
name2<-paste(sub(".txt","",datafiles[i],),"_",target_ID,".txt",sep="")
colnames(newID3)<-name2
rownames(newID3)<-newID2[,1]
write.table(newID3,name1,sep="\t",col.names=NA)
}else message(error)

#end of for loop


}
#end of error between 0 and 1 check 
}else {message("ERR Must lie between 0 and 1")}
#end of function
}
