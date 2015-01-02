\name{AALoess}
\alias{AALoess}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Normalises a set of gene expression data files using LOESS}
\description{This script normalises a set of gene expression data files against a predefined reference data set. The normalisation uses LOESS regression. The major assumption is that the actual data and the reference set have a similar epxression frequency distribution. Typically, the reference set used the is average of a large number of datasets such as the output of Baseline script in this package. The script also calculates the Sum of Squared Errror (SSE) between each data point of the normalised data set and the reference, and plots the results as a histogram of the SSE of all the data sets processed. This function is useful to identify outliers, which are usually due to problems in sample prepration/processing, which are frequently not identified by using the internal scanner quality performance indicators. 
}

\usage{
AALoess(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""), baseline=file.path(system.file(package="agilp"),"input1","testbase.txt"),LOG="TRUE")
}


\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory; the
	  input directory must contain ONLY the set of files to be processed. The input files must
  contain two columns only, the source identifier in the first column, and the expression data in
  the second column; see example files }
  \item{output}{full path of directory where output data files are put; default is a folder named output within the agilp package directory}
  \item{LOG}{if NORM="LOG",  the data are log base 2 transformed before normalisation. The default is NORM="LOG"}
  \item{baseline}{full path of file containing the reference data set for normalisation; the default is agilp/input1/baseline.txt} 
}

\value{\item{normalised datafiles}{One new file is written to the output folder for each input file. The format of the output files is the same as the input, but the data has been normalised. The normalised files are identified by the prefix nr_ or ng_}
         \item{SSE_date.txt file}{A file in the output folder containing a list of all the filenames in the input folder, and the corresponding SSE value}.
}


\author{Benny Chain; b.chain@ucl.ac.uk}
\seealso{\code{\link{AAProcess}}
	\code{\link{filenamex}}
	\code{\link{Loader}}
	\code{\link{Baseline}}
	\code{\link{IDswop}}
	\code{\link{Equaliser}}
}
 
\references{In preparation; for further detail on normalisation see also Chain B, Bowen H, Hammond J, Posch W, Rasaiyaah J, Tsang J, Noursadeghi M.
Error, reproducibility and sensitivity: a pipeline for data processing of Agilent
oligonucleotide expression arrays. BMC Bioinformatics. 2010 Jun 24;11:344.}

\examples{
#Takes four files of raw data (output of AAProcess, in dataset/raw folder) , LOess normalises them and saves them in output folder
inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)
outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)
baselinedir<-file.path(system.file(package="agilp"),"extdata","testbase.txt", fsep = .Platform$file.sep)
AALoess(input=inputdir, output=outputdir, baseline = baselinedir, LOG="TRUE") 
\dontrun{
#to remove these files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)
}
	}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{data}% __ONLY ONE__ keyword per line
