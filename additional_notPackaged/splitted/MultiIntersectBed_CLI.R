#--split MultiIntersectBed_CLI
#'@include CLIApplication
#'@title MultiIntersectBed_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{sortInFiles \code{"logical"}}
#'    \item{\code{slot7}:}{requireStrandness \code{"logical"}}
#'    \item{\code{slot8}:}{strandColumn \code{"numeric"}}
#'    \item{\code{slot9}:}{filterConditions \code{"character"}}
#'  }
#' @name MultiIntersectBed_CLI-class
#' @export
setClass("MultiIntersectBed_CLI", contains = "CLIApplication", representation(sortInFiles="logical", requireStrandness="logical", 
                                                                              strandColumn="numeric", filterConditions="character"))


#' @title Constructor method for MultiIntersectBed_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("MultiIntersectBed_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, sortInFiles,requireStrandness, strandColumn,filterConditions){standardGeneric("MultiIntersectBed_CLI")})

setMethod("MultiIntersectBed_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                        outputFlag="character", outFilePath="character",sortInFiles="logical",requireStrandness="logical", 
                                        strandColumn="numeric", filterConditions="character"),
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath,sortInFiles,requireStrandness, strandColumn, filterConditions ){
                                          return(new("MultiIntersectBed_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                                                     outputFlag=outputFlag, outFilePath=outFilePath, sortInFiles=sortInFiles,
                                                     requireStrandness=requireStrandness, strandColumn=strandColumn, filterConditions=filterConditions))
                                        })

setMethod("MultiIntersectBed_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                             outputFlag="character", outFilePath="character",sortInFiles="logical",requireStrandness="logical", 
                                             strandColumn="missing", filterConditions="character"),
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath,sortInFiles,requireStrandness, strandColumn, filterConditions ){
            strandColumn = 0
            return(new("MultiIntersectBed_CLI", inFilePath=inFilePath, inFileNames=inFileNames,  cliParams=cliParams, 
                       outputFlag=outputFlag, outFilePath=outFilePath, sortInFiles=sortInFiles,
                       requireStrandness=requireStrandness, strandColumn=strandColumn, filterConditions=filterConditions))
          })


#' @title Accessor getSortInFiles
#' @export
#' @docType methods
#' @return sortInFiles
setGeneric("getSortInFiles", function(object) standardGeneric("getSortInFiles"))
setMethod("getSortInFiles",signature(object="MultiIntersectBed_CLI"),function(object) {
  slot(object, "sortInFiles")
})

#' @title Accessor getRequireStrandness
#' @export
#' @docType methods
#' @return requireStrandness
setGeneric("getRequireStrandness", function(object) standardGeneric("getRequireStrandness"))
setMethod("getRequireStrandness",signature(object="MultiIntersectBed_CLI"),function(object) {
  slot(object, "requireStrandness")
})

#' @title Accessor getStrandColumn
#' @export
#' @docType methods
#' @return strandColumn
setGeneric("getStrandColumn", function(object) standardGeneric("getStrandColumn"))
setMethod("getStrandColumn",signature(object="MultiIntersectBed_CLI"),function(object) {
  slot(object, "strandColumn")
})


#' @title Accessor getFilterConditions
#' @export
#' @docType methods
#' @return filterConditions
setGeneric("getFilterConditions", function(object) standardGeneric("getFilterConditions"))
setMethod("getFilterConditions",signature(object="MultiIntersectBed_CLI"),function(object) {
  slot(object, "filterConditions")
})




#' @title Generates the commands and possibly resulting files/folders of Cutadapt
#' 
#' @param MultiIntersectBed_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="MultiIntersectBed_CLI"),function(object) { 
  #bedtools multiinter [OPTIONS] -i FILE1 FILE2 .. FILEn
  if( length(getInFilePath(object)) > 1 & length(getInFilePath(object)) != length(getInFileNames(object))  ){
    warning("Either one file path is given, containnig all files, or the file path vector must match the filename vector in length!")
  }

  inFN = getInFileNames(object)
  
  inFNtrimmed = sub("\\..+$","",inFN)
  inFilending = sub( inFNtrimmed[1], "", inFN[1]  )
  outFN = paste0( getOutputFlag(object), inFilending)
  
  if( getSortInFiles(object) ){
    message(" ... sorting inputFiles ... ")
    cmd1 = mapply( function(fp,fn) {
      tmpFileName = "testTmp321"
      cmdCurr = paste0("\nsort -k1,1 -k2,2n ",file.path(fp,fn), " > ", file.path(fp,tmpFileName) )
      cmdCurr = c(cmdCurr, paste0("cat ", file.path(fp,tmpFileName), " > ", file.path(fp,fn) ))
      cmdCurr = c(cmdCurr, paste0("rm ", file.path(fp,tmpFileName) ) )
      return( cmdCurr  )
    }, getInFilePath(object), getInFileNames(object) )
  } else{
    cmd1 = ""
  }
  
  if(getRequireStrandness(object)){
    if( length(getStrandColumn(object)) == 0 | getStrandColumn(object) < 1 ){
      stop("Column where the strand is defined in the Bed file has to be specified and needs to be greater than 0 when strandness is required!!")
    }
    tmpFileName = "testTmp321"
    message(" ... splitting input Files by strand ... ")
    cmd1 = c( cmd1, mapply( function(fp,fn) {
      cmdCurr = paste0("cat ", file.path(fp,fn), " | ", "awk \'{if($",getStrandColumn(object)," == \"-\") print }\' > " , file.path(fp,paste0("minus_",fn)) )
      cmdCurr = c( cmdCurr, paste0("cat ", file.path(fp,fn), " | ", "awk \'{if($",getStrandColumn(object)," == \"+\") print }\' > " , file.path(fp,paste0("plus_",fn)) ) )
      return( cmdCurr  )
    }, getInFilePath(object), getInFileNames(object) ) )
    
    cmd2 = paste0( "bedtools multiinter ",getCliParams(object), " -i ", paste0( file.path(getInFilePath(object), paste0("plus_",getInFileNames(object)) ),collapse=" "), " > ", 
                   file.path(getOutFilePath(object), paste0("plus_",outFN) ) )
    
    cmd2 = c(cmd2, paste0( "bedtools multiinter ",getCliParams(object), " -i ", paste0( file.path(getInFilePath(object), paste0("minus_",getInFileNames(object)) ),collapse=" "), " > ", 
                   file.path(getOutFilePath(object), paste0("minus_",outFN) ) ) )
    #Adding the minus sign to the multiinter file
    cmd2 = c(cmd2, paste0( "awk -Ft \'BEGIN {OFS=\"\\t\"}{print $0, \"-\"}\' ", file.path(getOutFilePath(object), paste0("minus_",outFN) ), " > ", file.path(getOutFilePath(object),tmpFileName) ) )
    cmd2 = c(cmd2, paste0("cat ", file.path(getOutFilePath(object),tmpFileName), " > ", file.path(getOutFilePath(object),paste0("minus_",outFN)) ))
    cmd2 = c(cmd2, paste0("rm ", file.path(getOutFilePath(object),tmpFileName) ) )
    
    cmd2 = c(cmd2, paste0( "awk -Ft \'BEGIN {OFS=\"\\t\"}{print $0, \"+\"}\' ", file.path(getOutFilePath(object), paste0("plus_",outFN) ), " > ", file.path(getOutFilePath(object),tmpFileName) ) )
    cmd2 = c(cmd2, paste0("cat ", file.path(getOutFilePath(object),tmpFileName), " > ", file.path(getOutFilePath(object),paste0("plus_",outFN)) ))
    cmd2 = c(cmd2, paste0("rm ", file.path(getOutFilePath(object),tmpFileName) ) )
        
    cmd2 = c(cmd2, paste0( "cat ", file.path(getOutFilePath(object), paste0("plus_",outFN)), " ", 
                                   file.path(getOutFilePath(object), paste0("minus_",outFN)),
                                   " | awk \'/./\' > ", file.path(getOutFilePath(object), outFN) ) ) #also removing blank lines from the file
    
    #sorting again
    cmd2 = c(cmd2, paste0("\nsort -k1,1 -k2,2n ",file.path(getOutFilePath(object), outFN), " > ", file.path(getOutFilePath(object),tmpFileName) ))
    cmd2 = c(cmd2, paste0("cat ", file.path(getOutFilePath(object),tmpFileName), " > ", file.path(getOutFilePath(object),outFN) ))
    cmd2 = c(cmd2, paste0("rm ", file.path(getOutFilePath(object),tmpFileName) ) )
  } else{
    cmd2 = paste0( "bedtools multiinter ",getCliParams(object), " -i ", paste0( file.path(getInFilePath(object), getInFileNames(object) ),collapse=" "), " > ", 
                   file.path(getOutFilePath(object),outFN ) )    
  }
  
  if( length(getFilterConditions(object)) > 1 ){
    
    generateFilterCmd = function(conditions, inputFileName, inputFilePath){
      #starting at column 6 in multiIntersectBed file
      conditions_u = unique(conditions)
      idx_conditions = lapply( conditions_u, function(x){which(conditions == x)})
      conditionStr = paste0( sapply( idx_conditions, function(x){
        if( length(x) > 1){
          paste0( paste0( "$",x, " > 0", sep=" && " ), collapse="")
        }  else{
          paste0( "$",x, " > 0")
        }
      } ), collapse=" || ")
      cmd1 = paste0( "\ncat ", file.path(inputFilePath,inputFileName), " > ", file.path(inputFilePath,paste0("UnFiltered_",inputFileName )))
      cmd2 = paste0( "\ncat ", file.path(inputFilePath, paste0("UnFiltered_", inputFileName)), 
                     " | awk \'{if( ", conditionStr,") print}\' > ", 
                     file.path(inputFilePath,inputFileName), "\n" )
      return(c(cmd1,cmd2))
    }
    
    cmd3 = generateFilterCmd(getFilterConditions(object),outFN, getOutFilePath(object))
    
  } else{
    cmd3 = ""
  }
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})
