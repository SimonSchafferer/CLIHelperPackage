#--split HTSeqCount_CLI

#@include CLIApplication
#'@title HTSeqCount_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{gffFile \code{"character"}}
#'  }
#' @name HTSeqCount_CLI-class
#' @export
setClass("HTSeqCount_CLI", contains = "CLIApplication", representation(gffFile="character"))

#' @title Accessor HTSeqCount_CLI
#' @export
#' @docType methods
#' @return gffFile
setGeneric("getGffFile", function(object){standardGeneric("getGffFile")})
setMethod("getGffFile",signature(object="HTSeqCount_CLI"),function(object) {
  slot(object, "gffFile")
})

#' @title Constructor method for HTSeqCount_CLI
#' @export
#' @docType methods
setGeneric("HTSeqCount_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, gffFile){standardGeneric("HTSeqCount_CLI")})
setMethod("HTSeqCount_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", gffFile="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, gffFile ){
            return(
              new("HTSeqCount_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, gffFile=gffFile)
            )
          })


#' @title Generates the commands and possibly resulting files/folders of HTSeqCount_CLI
#' 
#' @param HTSeqCount_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="HTSeqCount_CLI"),function(object) { 
#   Usage: htseq-count [options] alignment_file gff_file  
    
  inFN = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
  outFN = paste0( sub( "\\..*$","",inFN), getOutputFlag(object) )
  cmd2 = paste0( "htseq-count ", paste0(getCliParams(object),collapse=" "), " ", inFN, " ", getGffFile(object), " > ", file.path(getOutFilePath(object),outFN)  )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
  
})




