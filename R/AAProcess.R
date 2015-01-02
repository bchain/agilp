AAProcess<-function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output/",""),s=9){

#reads the input directory of file names into directory
directory<-unlist(dir(input))[]
M<-length(directory)

#read in first data set skipping first "s" rows

inputpath<-c(rep(1,M))
i<-1

for (i in 1:M)
{
inputpath[i]<-file.path(input,directory[i],fsep = .Platform$file.sep)

if(file.exists(inputpath[i])){
data<-read.table(inputpath[i],skip = s, header = FALSE, quote = "",comment.char = "", sep = "\t", fill = TRUE,  stringsAsFactors=FALSE )
colnames(data) <- data[1,]
data<-data[-1,]
probenames <- data[,"ProbeName"]
probes<-levels(factor(probenames))

green<-tapply(as.numeric(data[,"gMedianSignal"]),probenames,mean)
rownames(green)<-probes
Rawdg<-paste(output,"gRaw_",directory[i],sep = "")
greencol<-paste("gRaw_",directory[i],sep = "")
write.table(green,Rawdg,sep="\t", col.names = greencol, row.names = probes)

###############################################################################
if (match(as.vector("rMedianSignal"),as.vector(colnames(data)),nomatch=0)>0) {
red<-tapply(as.numeric(data[,"rMedianSignal"]),probenames,mean)
rownames(red)<-probes
Rawdr<-paste(output,"rRaw_",directory[i],sep = "")
redcol<-paste("rRaw_",directory[i],sep = "")
write.table(red,Rawdr,sep="\t", col.names = redcol, row.names = probes)
}
}else message("Array ",inputpath[i], "is not present")
#end of for loop
}
}
