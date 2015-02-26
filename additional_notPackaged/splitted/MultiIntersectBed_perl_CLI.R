#--split MultiIntersectBed_perl_CLI
#'@include IntersectBed_CLI
#'@title MultiIntersectBed_perl_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{groupVect \code{"numeric"}}
#'    \item{\code{slot7}:}{withinGroupTH \code{"numeric"}}
#'    \item{\code{slot8}:}{outFileName \code{"character"}}
#'    \item{\code{slot9}:}{perlPath \code{"character"}}
#'  }
#' @name MultiIntersectBed_perl_CLI-class
#' @export
setClass("MultiIntersectBed_perl_CLI", contains = "CLIApplication", representation(groupVect="numeric", withinGroupTH="numeric", outFileName="character", perlPath="character"))


#' @title Constructor method for MultiIntersectBed_perl_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
# setGeneric("MultiIntersectBed_perl_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, groupVect,withinGroupTH,outFileName){standardGeneric("MultiIntersectBed_perl_CLI")})
setGeneric("MultiIntersectBed_perl_CLI", function(inFilePath, inFileNames, outputFlag, outFilePath, outFileName, ...){standardGeneric("MultiIntersectBed_perl_CLI")})

setMethod("MultiIntersectBed_perl_CLI", signature(inFilePath="character", inFileNames="character", 
                                                  outputFlag="character", outFilePath="character",outFileName="character"),
          function(inFilePath, inFileNames, outputFlag, outFilePath, outFileName, cliParams="",withinGroupTH=0, groupVect=numeric(),perlPath=""){
             return(new("MultiIntersectBed_perl_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                        outputFlag=outputFlag, outFilePath=outFilePath, groupVect=groupVect,withinGroupTH=withinGroupTH, outFileName=outFileName, perlPath=perlPath ))
          })


# 
# setMethod("MultiIntersectBed_perl_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
#                                              outputFlag="character", outFilePath="character",groupVect="numeric",withinGroupTH="numeric",outFileName="character"),
#           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, groupVect, withinGroupTH, outFileName ){
#             cliParams=""
#             return(new("MultiIntersectBed_perl_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams="", 
#                        outputFlag=outputFlag, outFilePath=outFilePath, groupVect=groupVect,withinGroupTH=withinGroupTH, outFileName=outFileName ))
#           })
# setMethod("MultiIntersectBed_perl_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
#                                                   outputFlag="character", outFilePath="character",groupVect="numeric",withinGroupTH="missing",outFileName="character"),
#           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, groupVect, withinGroupTH, outFileName ){
#             cliParams=""
#             withinGroupTH = 0
#             return(new("MultiIntersectBed_perl_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams="", 
#                        outputFlag=outputFlag, outFilePath=outFilePath, groupVect=groupVect,withinGroupTH=withinGroupTH, outFileName=outFileName ))
#           })
# setMethod("MultiIntersectBed_perl_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
#                                                   outputFlag="character", outFilePath="character",groupVect="missing",withinGroupTH="numeric",outFileName="character"),
#           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, groupVect,withinGroupTH, outFileName ){
#             groupVect = numeric()
#             cliParams=""
#             return(new("MultiIntersectBed_perl_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
#                        outputFlag=outputFlag, outFilePath=outFilePath, groupVect=groupVect, withinGroupTH=withinGroupTH, outFileName=outFileName ))
#           })
# setMethod("MultiIntersectBed_perl_CLI", signature(inFilePath="character", inFileNames="character", cliParams="missing", 
#                                                   outputFlag="character", outFilePath="character",groupVect="missing",withinGroupTH="missing",outFileName="character"),
#           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, groupVect,withinGroupTH, outFileName ){
#             groupVect = numeric()
#             cliParams=""
#             withinGroupTH = 0
#             return(new("MultiIntersectBed_perl_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
#                        outputFlag=outputFlag, outFilePath=outFilePath, groupVect=groupVect, withinGroupTH=withinGroupTH, outFileName=outFileName ))
#           })

#' @title Accessor getGroupVect
#' @export
#' @docType methods
#' @return groupVect
setGeneric("getGroupVect", function(object){standardGeneric("getGroupVect")})
setMethod("getGroupVect",signature(object="MultiIntersectBed_perl_CLI"),function(object) {
  slot(object, "groupVect")
})

#' @title Accessor getPerlPath
#' @export
#' @docType methods
#' @return perlPath
setGeneric("getPerlPath", function(object){standardGeneric("getPerlPath")})
setMethod("getPerlPath",signature(object="MultiIntersectBed_perl_CLI"),function(object) {
  slot(object, "perlPath")
})

#' @title Accessor getWithinGroupTH
#' @export
#' @docType methods
#' @return withinGroupTH
setGeneric("getWithinGroupTH", function(object){standardGeneric("getWithinGroupTH")})
setMethod("getWithinGroupTH",signature(object="MultiIntersectBed_perl_CLI"),function(object) {
  slot(object, "withinGroupTH")
})

#' @title Accessor getOutFileName
#' @export
#' @docType methods
#' @return outFileName
setMethod("getOutFileName",signature(object="MultiIntersectBed_perl_CLI"),function(object) {
  slot(object, "outFileName")
})

#' @title Generates the commands and file for MultiIntersectBed
#' 
#' @param MultiIntersectBed_perl_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="MultiIntersectBed_perl_CLI"),function(object) { 
  #bedtools multiinter [OPTIONS] -i FILE1 FILE2 .. FILEn
  if( length(getInFilePath(object)) > 1 & length(getInFilePath(object)) != length(getInFileNames(object))  ){
    warning("Either one file path is given, containnig all files, or the file path vector must match the filename vector in length!")
  }
  if( length(getGroupVect(object)) > 1 & length(getGroupVect(object)) != length(getInFileNames(object))  ){
    warning("Either one file path is given, containnig all files, or the group vector must match the filename vector in length!")
  }
  
  inFN = getInFileNames(object)
  
  inFNtrimmed = sub("\\..+$","",inFN)
  inFilending = sub( inFNtrimmed[1], "", inFN[1]  )
  outFN = paste0( getOutFileName(object), inFilending)
  
  cmd1 = paste0("cd ", getOutFilePath(object))
  
  perlApp = ifelse(getPerlPath(object) == "", "perl", file.path(getPerlPath(object), "perl"))
  
  if(length(getGroupVect(object)) == 0){
    cmd2 = paste0( perlApp," ",system.file("data/multiIntersectBed_helper.pl",package="CLIHelperPackage"),
                   " -o ", file.path(getOutFilePath(object),outFN ),
                   " -w ", getWithinGroupTH(object), 
                   " -i ", paste0( file.path(getInFilePath(object), getInFileNames(object) ),collapse=" ") )    
  } else{
    cmd2 = paste0( perlApp," ",system.file("data/multiIntersectBed_helper.pl",package="CLIHelperPackage"),
                   " -o ", file.path(getOutFilePath(object),outFN ),
                   " -w ", getWithinGroupTH(object), 
                   " -g ", paste0(getGroupVect(object),collapse=" "),
                   " -i ", paste0( file.path(getInFilePath(object), getInFileNames(object) ),collapse=" ") )
  }
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)
})
