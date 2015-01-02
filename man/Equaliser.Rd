\name{Equaliser}
\alias{Equaliser}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Trims a set of gene expression data files to include only the set of identifiers common to all files
}
\description{This script processes a set of gene expression data files which  have different number of entries (i.e. different numbers of identifiers).  Further processing of data files usually requires equal number of identifiers in each file (e.g. AALoess or Baseline in this package). The Equaliser script cycles through a series of such datafiles, determines the set of  identifiers common to all files in the directory, and outputs a set of files which retain only those identifiers common to the whole set. 
}
\usage{
Equaliser(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""))
}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory; the input directory must contain ONLY the set of files to be processed. The input files must contain two columns only, the source identifier in the first column, and the expression data in the second column; see example files}
  \item{output}{full path of directory where output data files are put; default is a folder named output within the agilp package directory}
}


\value{
  \item{equalised datafiles}{One new file is produced for each input file. The file name has an additonal s_ as a suffix. }
	 }

\details{This utility is particularly useful to process the output of IDswop in the agilp package. IDswop excludes some identifiers on the basis of a cutoff which is data dependent, and may therefore generate files with different numbers of identifiers between samples. }

\author{Benny Chain; b.chain@ucl.ac.uk}
\seealso{\code{\link{AAProcess}}
	\code{\link{filenamex}}
	\code{\link{Loader}}
	\code{\link{Baseline}}
	\code{\link{IDswop}}
	\code{\link{AALoess}}
}
\references{In preparation}
\examples{
##Takes four files of raw data (output of AAProcess, in folder agilp/dataset/raw) selects all entries common to all four, and outputs the shortened files to the folder agilp/output.

inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)

outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)

Equaliser(input = inputdir, output = outputdir)

\dontrun{
#to remove these files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)

   }
  }


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{data}% __ONLY ONE__ keyword per line
