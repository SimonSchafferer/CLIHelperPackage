#--split MergeBedFile_CLI
#@include CLIApplication
#'@title MergeBedFile_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'  }
#' @name MergeBedFile_CLI-class
#' @export
setClass("MergeBedFile_CLI", contains = "CLIApplication")


#' @title Constructor method for MergeBedFile_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("MergeBedFile_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath){standardGeneric("MergeBedFile_CLI")})

setMethod("MergeBedFile_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath ){
                                      return(new("MergeBedFile_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
                                    })
setMethod("MergeBedFile_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath ){
                                      cliParams = "-s -c 4,5,6 -o mean,mean,distinct"
                                      return(new("MergeBedFile_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
                                    })

#' @title Generates the commands and possibly resulting files/folders of Cutadapt
#' 
#' @param MergeBedFile_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="MergeBedFile_CLI"),function(object) { 
  
  if( length(getInFileNames(object)) > 1 ){
    warning("More than one input file given, using the first one!")
  }
  
  inFN = getInFileNames(object)[1]
  cmd1 = paste0("cd ",getInFilePath(object))
  
  inFNtrimmed = sub("\\..+$","",inFN)
  inFilending = sub( inFNtrimmed, "", inFN  )
  outFN = paste0( inFNtrimmed, getOutputFlag(object), inFilending)
  
  cmd2 = paste0( "sort -k1,1 -k2,2n ", file.path(getInFilePath(object), getInFileNames(object) ), " | ", 
                 "mergeBed ", " -i stdin ", getCliParams(object), " | sort -k1,1 -k2,2n > ", file.path(getOutFilePath(object), outFN) )

  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
})

