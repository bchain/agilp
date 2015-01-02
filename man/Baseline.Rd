\name{Baseline}
\alias{Baseline}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Constructs a file with the mean  of each probe from a set of raw expression array data files} 

\description{Calculates the mean expression data for each probe or identifier  for a set of expression array data sets, for example as produced by AAProcess.  Identifiers (e.g. probe names) must lie in the first column of each file. Identifiers not common to all files in set are discarded. The set of files is either all files in input directory or a set of files defined using a template file. Template is typically an Excel spreadsheet saved as a tab delimited txt fiel, with file names as one column. The files to be used must be specified in a contiguous set of rows of the template file.
}
\usage{
Baseline(NORM="LOG",allfiles="TRUE",r=2,A=2,B=3,input=file.path(system.file(package="agilp"),"input",""),baseout=file.path(system.file(package="agilp"),"output","baseline.txt"),t=file.path(system.file(package="agilp"),"input1","template.txt"))
}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory}
  \item{t}{full name and path of template file; default is the file template.txt in folder agilp/input1}
  \item{baseout}{full name and path of output file with mean expression values; default is the file baseline.txt in folder agilp/output}
  \item{NORM}{if NORM="LOG",  the data are log base 2 transformed before the mean is calculated. The default is NORM="LOG"}
  \item{r}{The column of template which contains the file names}
  \item{A}{The first row of template to be used; note a header row is NOT ignored.}
  \item{B}{The last row of template to be used}
  \item{allfiles}{If allfiles is TRUE, the script uses all files in the input directory, and template is ignored; if allfiles=FALSE,
the scritp uses files defined by template
}
}
\value{
	 \item{baseout}{A tab delimited text file, with each identifier and its mean value,is written to the filename and path defined by baseout}
}

\details{This function is typically used as part of the data processing pipeline to calculate an average against which to normalise all data files using AALoessfunction }

\section{Warning}{The function checks that files listed in template exist in folder, and outputs a list of all file names not found}

\author{Benny Chain; b.chain@ucl.ac.ucl}
\seealso{\code{\link{AAProcess}}
	\code{\link{filenamex}}
	\code{\link{IDswop}}
	\code{\link{Loader}}
	\code{\link{Equaliser}}
	\code{\link{AALoess}}
}
\references{In preparation}
\examples{
#Takes four files of raw data in folder agilp/extdata/raw, calculates the mean expression value for each probe, and saves as agilp/output/testbase.txt.

inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)
outputbase<-file.path(system.file(package="agilp"),"output", "testbase.txt", fsep = .Platform$file.sep)
template<-file.path(system.file(package="agilp"),"extdata","sample_template.txt", fsep = .Platform$file.sep)
Baseline(NORM="LOG",allfiles="TRUE",r=2,A=2,B=3,input=inputdir, baseout=outputbase, t = template)

#Alternatively the following example uses only those data files defined in column 2, rows 2-5 of the template file. In additon, this example does not log transform the data.
Baseline(NORM="FALSE",allfiles="FALSE",r=2,A=2,B=5,input=inputdir, baseout=outputbase, t = template)

\dontrun{
#to remove the output files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{utilities}
\keyword{data}
% __ONLY ONE__ keyword per line
