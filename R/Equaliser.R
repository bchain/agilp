Equaliser <-
function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output","")){
arrays<-dir(input)
n<-length(arrays)

i<-1
name<-paste(input,arrays[i], sep="")
error<-paste("First file is absent so this data is not reliable")
if(file.exists(name)){
data<-read.table(file = name, row.names=1, header=TRUE, sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
}else message("First file is absent so this data is not reliable")


#########################################################################################################
#first pass runs though all files and finds common demoninator in file data
for (i in 2:n) {
name<-paste(input,arrays[i], sep="")
error<-paste("Arrays number",arrays[i], "is not present")
if(file.exists(name)){
data1<-read.table(file = name, row.names=1, header=TRUE, sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
common <- merge(data,data1,by.x="row.names",by.y="row.names")
data <-common[,-3]
data<-data.frame(data[,-1])
rownames(data)<-common[,1]
}else message("Array number ",arrays[i], " is not present")
}

#########################################################################################################
#second pass runs though all files again and merges with common demoninator and then saves with same name
for (i in 1:n) {

name<-paste(input,arrays[i], sep="")
if(file.exists(name)){
data1<-read.table(file = name, row.names=1, header=TRUE, sep = "\t", fill = TRUE,  stringsAsFactors=FALSE)
common <- merge(data,data1,by.x="row.names",by.y="row.names")

outfile<-data.frame(common[,3])
rownames(outfile)<-common[,1]
colnames(outfile)<-colnames(data1)

#Output
array_name<-arrays[i]
dg<-paste(output,"s_",array_name,sep = "")
write.table(outfile,dg,sep="\t",col.names=NA,row.names=TRUE)
} else message("Arrays number",arrays[i], "is not present")
#end of for loop inputing files
}
#end of function
}
