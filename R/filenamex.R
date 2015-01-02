filenamex <-function(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output","")){
names<-data.frame(dir(input))
namesout<-paste(output,"names.txt",sep="")
write.table(names,namesout,sep="\t",col.names=NA,row.names=TRUE)
###THE END
}
