#--split BamToBed_CLI

#'@include Samtools_CLI
#'@title BamToBed_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{outputFormat \code{"character"}}
#'  }
#' @name BamToBed_CLI-class
#' @export
setClass("BamToBed_CLI", contains = "CLIApplication", representation(outputFormat="character"))


#' @title Constructor method for BamToBed_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("BamToBed_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, outputFormat){standardGeneric("BamToBed_CLI")})

setMethod("BamToBed_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character", outputFormat="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, outputFormat ){
                                      return(new("BamToBed_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,
                                                 outFilePath=outFilePath, outputFormat=outputFormat))
                                    })
#' @title Accessor getOutputFormat
#' @export
#' @docType methods
#' @return outputFormat
setMethod("getOutputFormat", signature(object="BamToBed_CLI"), function(object){
  slot(object, "outputFormat")
})

#' @title Generates the commands and possibly resulting files/folders of BamToBed_CLI
#' 
#' @param BamToBed_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="BamToBed_CLI"),function(object) { 
  
  if( length(getInFileNames(object)) > 1 ){
    warning("More than one input file given, using the first one!")
  }
  
  inFN = getInFileNames(object)[1]
  cmd1 = paste0("cd ",getInFilePath(object))
  
  inFNtrimmed = sub("\\..+$","",inFN)
  outFN = paste0( inFNtrimmed, getOutputFlag(object),".", getOutputFormat(object))
  
  cmd2 = paste0( "bamToBed ", getCliParams(object)," -i ", file.path(getInFilePath(object), getInFileNames(object) ),
                 " > ", file.path(getOutFilePath(object), outFN) )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
})

