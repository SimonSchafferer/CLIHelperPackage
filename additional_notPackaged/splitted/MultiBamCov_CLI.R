#--split MultiBamCov_CLI

#@include CLIApplication
#'@title MultiBamCov_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{annotationFileMB \code{"character"}}
#'    \item{\code{slot7}:}{annotationType \code{"character"}}
#'  }
#' @name MultiBamCov_CLI-class
#' @export
setClass("MultiBamCov_CLI", contains = "CLIApplication", representation(annotationFileMB="character", annotationType = "character"))

#' @title Accessor annotationFileMB
#' @export
#' @docType methods
#' @return annotationFileMB
setGeneric("getAnnotationFileMB", function(object){standardGeneric("getAnnotationFileMB")})
setMethod("getAnnotationFileMB",signature(object="MultiBamCov_CLI"),function(object) {
  slot(object, "annotationFileMB")
})
#' @title Accessor annotationType
#' @export
#' @docType methods
#' @return annotationType
setGeneric("getAnnotationType", function(object){standardGeneric("getAnnotationType")})
setMethod("getAnnotationType",signature(object="MultiBamCov_CLI"),function(object) {
  slot(object, "annotationType")
})

#' @title Constructor method for MultiBamCov_CLI
#' @export
#' @docType methods
setGeneric("MultiBamCov_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB, annotationType){standardGeneric("MultiBamCov_CLI")})
setMethod("MultiBamCov_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                      outputFlag="character", outFilePath="character", annotationFileMB="character", annotationType="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB,annotationType ){
            return(
              new("MultiBamCov_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, annotationFileMB=annotationFileMB,annotationType=annotationType)
            )
          })

setMethod("MultiBamCov_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="character", 
                                       outputFlag="character", outFilePath="character", annotationFileMB="character", annotationType="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB,annotationType ){
            inFileNames = list.files(path = inFilePath, pattern = ".*\\.sam$")
            return(
              new("MultiBamCov_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, annotationFileMB=annotationFileMB,annotationType=annotationType)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of MultiBamCov_CLI
#' 
#' @param MultiBamCov_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="MultiBamCov_CLI"),function(object) { 
  #   Usage: bedtools multicov [OPTIONS] -bams aln.1.bam aln.2.bam ... aln.n.bam -bed <bed/gff/vcf>
  
  inFNs = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
#   outFN = paste0( gsub(".*/","",sub( "\\..*$","",inFNs[1])), getOutputFlag(object) )
  outFN = paste0( getOutputFlag(object) )
  
  cmd2 = paste0( "bedtools multicov ", paste0(getCliParams(object),collapse=" "), " -bams ", paste0(inFNs, collapse=" "), " -",
                 getAnnotationType(object)," ", getAnnotationFileMB(object), " > ",  file.path(getOutFilePath(object),outFN)  )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
  
})



