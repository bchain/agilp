\name{AAProcess}
\alias{AAProcess}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracts raw expression data from Agilent expression array scanner files.}

\description{This function takes scanner "txt format" files produced by the Agilent microarray scanner, extracts just the raw median intensity values from the columns marked "gMedianSignal" or "rMedianSignal". Replicate probes  are averaged. The expression data for each colour is written as separate txt tab delimited files with the same name as the original plus prefix rRaw_ or gRaw_. Probe names are in the first column, expression values in the second column. 
}
\usage{
AAProcess(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output/",""),s=9)

}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory}
  \item{output}{full path of directory where output data files are put; default is a folder named output within the agilp package directory}
  \item{s}{number of header lines to skip when reading in scanner files. Default is 9}
}
\value{
	 \item{rawdata files}{ The function writes a set of tab delimited txt files containing the raw data for each channel to directory defined by output. The raw data files are given the same name as the input data files with the prefix Rawg and Rawr for green and red channel respectively. The output files can be readily opened in Excel.  In R each file is a dataframe, which contains probe names in first column, and expression data in second column. Replicate probes are averaged.} 
}

\details{The input directory must have ONLY the data files to be analysed.}
\author{Benny Chain; b.chain@ucl.ac.uk}
\seealso{\code{\link{Loader}}
	\code{\link{filenamex}}
	\code{\link{IDswop}}
	\code{\link{Baseline}}
	\code{\link{Equaliser}}
	\code{\link{AALoess}}
}
\references{In preparation}

\examples{dir()

#This examples extracts expression data from two sample array scanner files which are found in the folder "scanner" within the agilp package extdata folder
#and places them in the folder output.
inputdir<-file.path(system.file(package="agilp"),"extdata","scanner","", fsep = .Platform$file.sep)
outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)
AAProcess(input = inputdir, output = outputdir, s = 9)

\dontrun{
#to remove these files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output\\",""),"*.*",sep=""), recursive=FALSE)

#The number of lines header to be skipped from scanner array files can be changed from the default value of 9 by setting variable s.
AAProcess(input = inputdir, output = outputdir, s = 12)
   }
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{data}% __ONLY ONE__ keyword per line
