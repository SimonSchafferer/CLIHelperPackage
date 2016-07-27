#--split Bowtie2_CLI
#################################################
#     General generic function definitions that are implemented in several subclasses!
#################################################

#@include CLIApplication
#'@title Bowtie2_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{bowtieIndexFilePath \code{"character"}}
#'    \item{\code{slot7}:}{matepairFileNames \code{"character"}}
#'  }
#' @name Bowtie2_CLI-class
#' @export
setClass("Bowtie2_CLI", contains = "CLIApplication", representation(bowtieIndexFilePath="character", matepairFileNames="character") )

#' @title Accessor getBowtieIndexFilePath
#' @export
#' @docType methods
#' @return bowtieIndexFilePath
setGeneric("getBowtieIndexFilePath", function(object) standardGeneric("getBowtieIndexFilePath"))
setMethod("getBowtieIndexFilePath",signature(object="Bowtie2_CLI"),function(object) {
  slot(object, "bowtieIndexFilePath")
})

#' @title Accessor getMatepairFileNames
#' @export
#' @docType methods
#' @return matepairFileNames
setGeneric("getMatepairFileNames", function(object){standardGeneric("getMatepairFileNames")})
setMethod("getMatepairFileNames",signature(object="Bowtie2_CLI"),function(object) {
  slot(object, "matepairFileNames")
})

#' @title Constructor method for Bowtie2_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("Bowtie2_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath, matepairFileNames){standardGeneric("Bowtie2_CLI")})

setMethod("Bowtie2_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", matepairFileNames="missing"), 
         function(inFilePath, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames){
           inFileNames = list.files(path = inFilePath, pattern = ".*\\.fastq$")
           return(
             new("Bowtie2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                 outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath,matepairFileNames="")
           )
         })
setMethod("Bowtie2_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", matepairFileNames="character"), 
          function(inFilePath, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames){
            inFileNames = list.files(path = inFilePath, pattern = ".*\\.fastq$")
            return(
              new("Bowtie2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath,matepairFileNames=matepairFileNames)
            )
          })

setMethod("Bowtie2_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", matepairFileNames="missing"), 
           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames ){
             return(
               new("Bowtie2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                   outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames="")
             )
          })
setMethod("Bowtie2_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", matepairFileNames="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames ){
            return(
              new("Bowtie2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames=matepairFileNames)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of Bowtie2
#' 
#' @param Bowtie2_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Bowtie2_CLI"),function(object) { 
 #USAGE: bowtie2 [options]* -x <bt2-idx> {-1 <m1> -2 <m2> | -U <r>} [-S <sam>]
  
  allFiles = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
  allFilesOut = paste0( sub("\\.fastq$","",allFiles), getOutputFlag(object), ".fastq")
  
#   dir.create(getOutFilePath(object))
  cmd2 = paste0("mkdir ", getOutFilePath(object)) #if only executed in terminal!
  
  if( getMatepairFileNames(object)[1] == "" ){
    cmd3 = paste0("bowtie2 ", paste0(getCliParams(object),collapse=" "), " -x ", getBowtieIndexFilePath(object), " -U ", allFiles, " -S ",
                  file.path(getOutFilePath(object), allFilesOut) )
  } else{
    cmd3 = paste0("bowtie2 ", paste0(getCliParams(object),collapse=" "), " -x ", getBowtieIndexFilePath(object), " -1 ", allFiles,
                 " -2 ",getMatepairFileNames(object), " -S ", file.path(getOutFilePath(object), allFilesOut) )
  }
                
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(allFilesOut), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})

