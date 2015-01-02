\name{IDswop}
\alias{IDswop}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Mapping expression data across bioinformatic identifiers
}
\description{This script maps expression data associated with a source identifier (typically a platform
	specific identifier such as ProbeID, as output by AAProcess in the agilp package for example), to another
	identifier of the user's choice. The script uses an annotation file which contains the
	mappings between identifiers. Each column denotes a different identifier, which is
	shown in the column heading. Typical examples include ProbeID, EntrezGeneID,
	EnsemblTranscriptID etc.) Annotation files are provided by microarray manufacturers,or,
	preferably are created using, for example the biomaRt package in Bioconductor. 
}
\usage{
IDswop(input=file.path(system.file(package="agilp"),"input",""),output=file.path(system.file(package="agilp"),"output",""),fun="mean",annotation=file.path(system.file(package="agilp"),"input1","annotation.txt"), source_ID = "ProbeID", target_ID="EnsemblID", ERR=0.2)
}

\arguments{
  \item{input}{full path of directory where input data files are put; default is a folder named input within the agilp package directory; the
	  input directory must contain ONLY the set of files to be processed. The input files must contain two columns only, the source identifier in the first column, and the expression data in
  the second column; see example files in datasets folder}
  \item{output}{full path of directory where output data files are put; default is a folder named input within the agilp package directory}
  \item{fun}{the function to be applied in cases where multiple probes map to the same
	  identifier; currently allowed values are "mean" or "max".}
  \item{annotation}{full path of annotation file; default is a file called annotation.txt within a folder named input1 within the agilp package directory}
  \item{source_ID}{The identifier from which the data is mapped. This must appear exactly as it
	  appears in the heading of the appropriate  column of the annotation file.} 
  \item{target_ID}{The identifier to which the data is mapped. This must appear exactly as it
	  appears in the heading of the appropriate  column of the annotation file}
  \item{ERR}{The cut-off for coefficient of variation allowed when multiple probes map to a
	  single identifier. ERR must lie between 0 and 1. If ERR=0, any difference
	  between such probes will result in the identifier being omitted from the final list.  If
	  ERR=1,  all identifiers are retained. The default is 0.2, allowing a
	  maximum coefficient of  variation between multiple probes mapping to a single
	  identifier of 20\%. }
	  }
\value{
	 \item{remapped datafiles}{One new file is produced for each input file. Each file contains two columns; the first contains the new set of identifiers; the second contains the corresponding expression value. The file name contains the new identifier as a suffix. }
	 
}

\details{This function is designed for merging expression data sets from different
	platforms using common bioinformatic identifiers such as gene names, transcript names etc.
	The major problem lies in how to deal with the "many to many" mapping which
	characterise the relationships between most standard identifiers. IDswop deals with the problem as follows. If	multiple source identifiers (typically platform specific probe IDs) map to a single target (for example Ensembl_Transcript_ID ) , the different probes are either
	averaged (fun="mean") or the maximum is selected (fun="max"). However, in cases where the discrepancy between such sets of probes is too large, the identifier is discarded,
	since it is considered as being unreliable. The degree of error allowed can be set by the user, as described under a descrition of the ERR variable given above. In cases where a
	single source identifier maps to multple targets (e.g. one probe to several
	transcript IDs) each transcript is retained as a separate entry in the final list, with an identical expression value. }

\author{Benny Chain; b.chain@ucl.ac.uk}
\seealso{\code{\link{AAProcess}}
	\code{\link{filenamex}}
	\code{\link{Loader}}
	\code{\link{Baseline}}
	\code{\link{Equaliser}}
	\code{\link{AALoess}}
}
\references{In preparation}
\examples{

#This examples maps expression data from Agilent ProbeIDs (found in column one of input files) to corresponding EnsemblID. Each output file has the name of the original file with the suffix "_EnsemblID". If multiple probes map to teh same ID and vary by more than 20% they are discarded.


inputdir<-file.path(system.file(package="agilp"),"extdata","raw/","", fsep = .Platform$file.sep)
outputdir<-file.path(system.file(package="agilp"),"output/", "", fsep = .Platform$file.sep)
annotation<-file.path(system.file(package="agilp"),"extdata","annotations_sample.txt", fsep = .Platform$file.sep)
IDswop(input=inputdir,output=outputdir,annotation=annotation,source_ID="ProbeID",target_ID="EnsemblID", ERR=0.2)

#Alternatively the following keeps all output IDs, even when multiple probes map to a single

IDswop(input=inputdir,output=outputdir,annotation=annotation,source_ID="ProbeID",target_ID="EnsemblID", ERR=1)

\dontrun{
#to remove the output files again and empty the output directory use 
unlink(paste(file.path(system.file(package="agilp"),"output",""),"*.*",sep=""), recursive=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{data}% __ONLY ONE__ keyword per line
