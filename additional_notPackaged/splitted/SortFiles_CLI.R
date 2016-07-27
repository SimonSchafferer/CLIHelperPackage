#--split SortFile_CLI
#@include CLIApplication
#'@title SortFile_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'  }
#' @name SortFile_CLI-class
#' @export
setClass("SortFile_CLI", contains = "CLIApplication")

#' @title Constructor method for SortFile_CLI
#' @param inFilePath only one filename allowed!
#' @export
#' @docType methods
setGeneric("SortFile_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath){standardGeneric("SortFile_CLI")})

setMethod("SortFile_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath ){
                                      return(new("SortFile_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
                                    })
setMethod("SortFile_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath ){
                                      cliParams = "-k1,1 -k2,2n"
                                      return(new("SortFile_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
                                    })

#' @title Generates the commands and possibly resulting files of linux sort command SortFile_CLI
#' 
#' @param SortFile_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="SortFile_CLI"),function(object) { 
  
  if( length(getInFileNames(object)) > 1 ){
    warning("More than one input file given, using the first one!")
  }
  
  inFN = getInFileNames(object)[1]
  cmd1 = paste0("cd ",getInFilePath(object))
  
  inFNtrimmed = sub("\\..+$","",inFN)
  inFilending = sub( inFNtrimmed, "", inFN  )
  
  outFN = paste0( inFNtrimmed, getOutputFlag(object), inFilending)
  
  cmd2 = paste0( "sort ", getCliParams(object), " ", file.path(getInFilePath(object), getInFileNames(object) ), " > ", file.path(getOutFilePath(object), outFN)   )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
})
