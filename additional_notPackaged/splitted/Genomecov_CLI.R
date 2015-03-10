#--split Genomecov_CLI
# Needs to be placed after IntersectBed_CLI
#'@include CLIApplication
#'@title Genomecov_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}} In this class the flag may be bedgraph (not used but specifies filetype)
#'    \item{\code{slot6}:}{outFileName \code{"character"}}
#'    \item{\code{slot7}:}{inFileBam \code{"logical"}}
#'  }
#' @name Genomecov_CLI-class
#' @export
setClass("Genomecov_CLI", contains = "CLIApplication", representation(outFileName="character", inFileBam="logical"))

#' @title Constructor method for Genomecov_CLI
#' @param inFilePath only one filename allowed!
#' @export
#' @docType methods
setGeneric("Genomecov_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, outFileName, inFileBam){standardGeneric("Genomecov_CLI")})

setMethod("Genomecov_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                        outputFlag="character", outFilePath="character", outFileName="character", 
                                     inFileBam="logical"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, 
                                                                                                                            outFileName,inFileBam ){
                                          return(new("Genomecov_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,
                                                     outFilePath=outFilePath, outFileName=outFileName, inFileBam=inFileBam))
                                        })

#' @title Accessor getOutFileName
#' @export
#' @docType methods
#' @return outFileName
setMethod("getOutFileName",signature(object="Genomecov_CLI"),function(object) {
  slot(object, "outFileName")
})

#' @title Accessor getInFileBam
#' @export
#' @docType methods
#' @return inFileBam
setGeneric("getInFileBam", function(object){standardGeneric("getInFileBam")})
setMethod("getInFileBam",signature(object="Genomecov_CLI"),function(object) {
  slot(object, "inFileBam")
})


#' @title Generates the commands and possibly resulting files of linux sort command Genomecov_CLI
#' 
#' @param Genomecov_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Genomecov_CLI"),function(object) { 
  
  if( length(getInFileNames(object)) > 1 ){
    warning("Only one file allowed, the first one will be chosen!")
  } 
  
  inFN = getInFileNames(object)[1]
  outFN = getOutFileName(object)
  inFN_path = getInFilePath(object)
  
  cmd1 = paste0("cd ",getInFilePath(object))
  
  cmd2 = paste0( "bedtools genomecov ", getCliParams(object), ifelse( getInFileBam(object), " -ibam ", " -i " ) ,file.path(inFN_path, inFN), " > ", 
                 file.path(getOutFilePath(object), outFN) )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
})

