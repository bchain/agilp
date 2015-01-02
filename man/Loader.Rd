\name{Loader}
\alias{Loader}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{A file choser utility file.}
\description{Uses a template file (typically an Excel spreadsheet with file names as one column) to select a set of files and either combine them into one file or copy them individually to a new directory. The files to be used must be specified in a contiguous set of rows of the template file.
}
\usage{
Loader(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""),t=file.path(system.file(package="agilp"),"input1","template.txt"),f="TRUE",r=2,A=2,B=5)
}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory;}
  \item{output}{full path of directory where output data files are put; default is a folder named output within the agilp package directory;}
  \item{t}{full name and path of template file}
  \item{f}{if f="TRUE",  each file is copied individually to output folder; if "f" IS NOT true, all files are merged into one big dataframe , with each original file data as column; this file is called "all_data.txt"  and written to  output folder; it is also returned as an R dataframe object called dataout }
  \item{r}{The column of template which contains the file names}
  \item{A}{The first row of template to be used; note a header row is NOT ignored.}
  \item{B}{The last row of template to be used}
}
\value{
	 \item{rawdata files}{If f is "TRUE" the function simply copies the files defined by template from input to output folder. }
	 \item{dataout}{If f is not = "TRUE" , the individual data files are merged by rows. The rownames are the probe names; each column contains a set of expression data with the column name giving the file name from which the data were derived}
	 \item{all_data}{If f is not = "TRUE" the function also writes a tab delimited txt file corresponding to dataout, called all_data.txt to the output directory. } 
}

\details{This function is a tool for working with selected files output by AAProcess. Typical usage would be to hold all the array file names with relevant experimental detail and annotation in a file called template. The filenames would then be sorted according to some annoation (e.g. tissue type, date, experimenter etc and then this list is used to pull out the relevant raw data files corresponding to those epxeriments from a big collection of such files. A toy example of a template file is provided in data folder}
\section{Warning}{The function checks that files listed in template exist in folder, and outputs a list of all file names not found}
\author{Benny Chain; b.chain@ucl.ac.ucl}
\seealso{\code{\link{AAProcess}}
	\code{\link{filenamex}}
	\code{\link{IDswop}}
	\code{\link{Baseline}}
	\code{\link{Equaliser}}
	\code{\link{AALoess}}
}
\references{In preparation}
\examples{

inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)
outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)
template<-file.path(system.file(package="agilp"),"extdata","sample_template.txt", fsep = .Platform$file.sep)

#This will copy the files with names given in column 2 , rows 2:3 of the sample_#template file from input to output. 
Loader(input=inputdir,output=outputdir,t=template,f="TRUE",r=2,A=2,B=5)

#Alternatively this will output a single file all_data.txt with the same data in a merged file. The file is also output back to R as object dataout
Loader(input=inputdir,output=outputdir,t=template,f="FALSE",r=2,A=2,B=5)

dim(dataout)

\dontrun{
#to remove the output files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{data}% __ONLY ONE__ keyword per line
