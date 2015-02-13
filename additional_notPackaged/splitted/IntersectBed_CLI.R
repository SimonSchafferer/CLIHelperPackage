#--split IntersectBed_CLI
#'@include CLIApplication
#'@title IntersectBed_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{outFileName \code{"character"}}
#'  }
#' @name IntersectBed_CLI-class
#' @export
setClass("IntersectBed_CLI", contains = "CLIApplication", representation(outFileName="character"))

#' @title Constructor method for IntersectBed_CLI
#' @param inFilePath only one filename allowed!
#' @export
#' @docType methods
setGeneric("IntersectBed_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, outFileName){standardGeneric("IntersectBed_CLI")})

setMethod("IntersectBed_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character", outFileName="character"), function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, 
                                                                                               outFileName ){
                                      return(new("IntersectBed_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, outputFlag=outputFlag,
                                                 outFilePath=outFilePath, outFileName=outFileName))
                                    })

#' @title Accessor getOutFileName
#' @export
#' @docType methods
#' @return outFileName
setGeneric("getOutFileName", function(object){standardGeneric("getOutFileName")})
setMethod("getOutFileName",signature(object="IntersectBed_CLI"),function(object) {
  slot(object, "outFileName")
})

#' @title Generates the commands and possibly resulting files of linux sort command IntersectBed_CLI
#' 
#' @param IntersectBed_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="IntersectBed_CLI"),function(object) { 
  
  if( length(getInFileNames(object)) > 2 ){
    message("First File will be taken as a and all others as b")
  } else if( length(getInFileNames(object)) < 2 ){
    stop("Please provide at least two files as input!")
  } 

  inFN_a = getInFileNames(object)[1]
  inFN_b = getInFileNames(object)[-1]
    
  if( length(getInFilePath(object)) == 1  ){
    message("All files have to be present in the inFilePath directory!")
    inFN_a_path = getInFilePath(object)
    inFN_b_path = getInFilePath(object)
  } else if( length(getInFilePath(object)) != length(getInFileNames(object)) ){
    stop("Please provide the same number of paths to the input files as input file names")
  } else{
    inFN_a_path = getInFilePath(object)[1]
    inFN_b_path = getInFilePath(object)[-1]
  }
  
  cmd1 = paste0("cd ",getInFilePath(object))
  
  inFNtrimmed = sub("\\..+$","",inFN_a)
  inFilending = sub( inFNtrimmed, "", inFN_a  )
  
  cmd2 = paste0( "mkdir ", getOutFilePath(object) )
  
  outFN = paste0( getOutFileName(object), getOutputFlag(object), inFilending)
  
  cmd3 = paste0( "intersectBed ", getCliParams(object), " -a ", file.path(inFN_a_path, inFN_a), " -b ", 
                 paste0( file.path(inFN_b_path, inFN_b), collapse=" "), " > ", file.path(getOutFilePath(object), outFN)  )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})


