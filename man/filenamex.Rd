\name{filenamex}
\alias{filenamex}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{A file name listing utility} 

\description{Generates a text file with the name of all files in a given directory and saves it as a tab delimited txt file. Helpful for filling in template files (see Loader) without making mistakes}

\usage{filenamex(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""))
}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory}
  \item{output}{full path of directory where output data files are put; default is a folder named output within the agilp package directory}
}
\value{
	 \item{names.txt}{A tab delimited text file with the names of all the files in the input directory}
}
\details{None}

\author{Benny Chain; b.chain@ucl.ac.ucl}
\seealso{\code{\link{AAProcess}}
	\code{\link{Loader}}
	\code{\link{IDswop}}
	\code{\link{Baseline}}
	\code{\link{Equaliser}}
	\code{\link{AALoess}}
}
\references{In preparation}
\examples{
#This example makes a list of files in the folder agilp/extdata/raw and saves it in a file called names.txt (tab delimited) in the folder agilp/output.
inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)
outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)
filenamex(input=inputdir,output=outputdir)

\dontrun{
#to remove these files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)

   }
}

\keyword{file}% __ONLY ONE__ keyword per line
