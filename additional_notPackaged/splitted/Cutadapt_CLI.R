#--split Cutadapt_CLI
#'@include CLIApplication
#'@title Cutadapt_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'  }
#' @name Cutadapt_CLI-class
#' @export
setClass("Cutadapt_CLI", contains = "CLIApplication")


#' @title Constructor method for Cutadapt_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("Cutadapt_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath){standardGeneric("Cutadapt_CLI")})

setMethod("Cutadapt_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="character", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, cliParams, outputFlag, outFilePath){
  inFileNames = list.files(path = inFilePath, pattern = ".*\\.fastq$")
  return(new("Cutadapt_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
})
setMethod("Cutadapt_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath ){
  return(new("Cutadapt_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,outFilePath=outFilePath))
})

#' @title Generates the commands and possibly resulting files/folders of Cutadapt
#' 
#' @param Cutadapt_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Cutadapt_CLI"),function(object) { 
  allFiles = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
  allFilesOut = paste0( sub("\\.fastq$","",allFiles) ,getOutputFlag(object), ".fastq")
#   dir.create(getOutFilePath(object))
  cmd2 = paste0("mkdir ", getOutFilePath(object))
  cmd3 = paste0("cutadapt ", paste0(getCliParams(object),collapse=" "), " -o ", file.path(getOutFilePath(object), allFilesOut), " ", allFiles)
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(allFilesOut), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})

