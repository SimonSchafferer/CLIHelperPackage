#'@title CLIApplication Virtual Class
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'  }
#' @name CLIApplication-class
#' @export
setClass("CLIApplication", representation(inFilePath = "character", 
                                          inFileNames = "character", 
                                          cliParams = "character", 
                                          outFilePath = "character",
                                          outputFlag = "character",
                                          "VIRTUAL"))

#' @title Generates the commands and possibly resulting files/folders
#' @param CLIApplication
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setGeneric("generateCommandResult", function( object ) { 
  value = standardGeneric("generateCommandResult") 
  if( !is(value, "CmdGenResult") )
    stop("Return Value must be CmdGenResult!")
  value
  })

#' @title Accessor getInFilePath
#' @export
#' @docType methods
#' @return inFilePath
setGeneric("getInFilePath", function(object) standardGeneric("getInFilePath"))
setMethod("getInFilePath",signature(object="CLIApplication"),function(object) {
  slot(object, "inFilePath")
})
#' @title Accessor getInFileNames
#' @export
#' @docType methods
#' @return inFileNames
setGeneric("getInFileNames", function(object) standardGeneric("getInFileNames"))
setMethod("getInFileNames",signature(object="CLIApplication"),function(object) {
  slot(object, "inFileNames")
})
#' @title Accessor getCliParams
#' @export
#' @docType methods
#' @return cliParams
setGeneric("getCliParams", function(object) standardGeneric("getCliParams"))
setMethod("getCliParams",signature(object="CLIApplication"),function(object) {
  slot(object, "cliParams")
})
#' @title Accessor getOutFilePath
#' @export
#' @docType methods
#' @return outFilePath
setGeneric("getOutFilePath", function(object) standardGeneric("getOutFilePath"))
setMethod("getOutFilePath",signature(object="CLIApplication"),function(object) {
  slot(object, "outFilePath")
})

#' @title Accessor getOutputFlag
#' @export
#' @docType methods
#' @return outputFlag
setGeneric("getOutputFlag", function(object) standardGeneric("getOutputFlag"))
setMethod("getOutputFlag",signature(object="CLIApplication"),function(object) {
  slot(object, "outputFlag")
})


#################################################
#     General generic function definitions that are implemented in several subclasses!
#################################################

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


#'@title Bowtie2TophatIon_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{bowtieIndexFilePath \code{"character"}}
#'    \item{\code{slot7}:}{matepairFileNames \code{"character"}}
#'    \item{\code{slot8}:}{outFileNames \code{"character"}}
#'    \item{\code{slot9}:}{pathToPicardTools \code{"character"}}
#'  }
#' @name Bowtie2TophatIon_CLI-class
#' @export
setClass("Bowtie2TophatIon_CLI", contains = "Bowtie2_CLI", representation(outFileNames="character", pathToPicardTools="character"), prototype(outFileNames = "unmapped"))

# @title Validity method for Bowtie2TophatIon_CLI
# @name validityBowtie2TophatIon_CLI
# @rdname validityBowtie2TophatIon_CLI
# @export
# @docType methods
setValidity ("Bowtie2TophatIon_CLI",
             function ( object ){
               retval = NULL
               if(length(getInFilePath(object)) > 1){ retval = "Multiple In File Paths (Tophat output directories are not allowed!)" }                          
               if ( is.null(retval)){
                 return(TRUE)
               }
               else{
                 return(retval)
               }
             })

#' @title Constructor method for Bowtie2TophatIon_CLI
#' @export
#' @docType methods
setGeneric("Bowtie2TophatIon_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames, outFileNames, pathToPicardTools){standardGeneric("Bowtie2TophatIon_CLI")})
setMethod("Bowtie2TophatIon_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="missing", 
                                   outputFlag="character", outFilePath="missing", bowtieIndexFilePath="character", matepairFileNames="missing", 
                                   outFileNames="missing", pathToPicardTools="missing"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames,outFileNames, pathToPicardTools){
                            
            warning("Options are locked and input file name is set to unmapped, outfile is set to bam mate pairs are not supported!")
            if(missing(outFileNames)){outFileNames = "aligned"}
            if(missing(pathToPicardTools)){pathToPicardTools = "/usr/local/applications/picard-tools-1.77/"}
            if(missing(outFilePath)){outFilePath = inFilePath; warning("outFilePath is set to inFilePath")}
            
            return(
              new("Bowtie2TophatIon_CLI", inFilePath=inFilePath, inFileNames="unmapped", cliParams=c("--local","--very-sensitive-local","-p 8","--mm"), 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames="", 
                  outFileNames = outFileNames, pathToPicardTools=pathToPicardTools)
            )
          })

#' @title Accessor getOutFileNames
#' @export
#' @docType methods
#' @return outFileNames
setGeneric("getOutFileNames", function(object){standardGeneric("getOutFileNames")})
setMethod("getOutFileNames",signature(object="Bowtie2_CLI"),function(object) {
  slot(object, "outFileNames")
})

#' @title Accessor getPathToPicardTools
#' @export
#' @docType methods
#' @return pathToPicardTools
setGeneric("getPathToPicardTools", function(object){standardGeneric("getPathToPicardTools")})
setMethod("getPathToPicardTools",signature(object="Bowtie2_CLI"),function(object) {
  slot(object, "pathToPicardTools")
})


#' @title Generates the commands and possibly resulting files/folders of Bowtie2 in combination with Tophat output for ion Torrent
#' 
#' @param Bowtie2TophatIon_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Bowtie2TophatIon_CLI"),function(object) { 
  #USAGE: bowtie2 [options]* -x <bt2-idx> {-1 <m1> -2 <m2> | -U <r>} [-S <sam>]
  #When Tophout2 results are created, bam2fastq is callled, then bowtie 2 is called, results are merged by picard tools MergeSamFiles
  allFiles = getInFileNames(object)
  
    inFP = getInFilePath(object)
    
    cmd1 = paste0("cd ",inFP)
    outFile = paste0( getOutFileNames(object), getOutputFlag(object), ".bam")  
  
    cmd2 = paste0("bam2fastq -o unmapped.fastq unmapped.bam")
    cmd3 = paste0("bowtie2 ", paste0(getCliParams(object),collapse=" "), " -x ", getBowtieIndexFilePath(object), " -U ", "unmapped.fastq", 
                  " | samtools view -uhS -F4 - | samtools sort - unmapped_remap")
    
    cmd4 = paste0("java -jar ", file.path( getPathToPicardTools(object), "MergeSamFiles.jar"),
                  " USE_THREADING=true MSD=true AS=true I=accepted_hits.bam I=unmapped_remap.bam O=",file.path(getOutFilePath(object), outFile) )
        
    res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(file.path(getOutFilePath(object), outFile)), commands = c(cmd1, cmd2, cmd3, cmd4))
    return(res)
  } )

#'@title OutResultReference Virtual Class
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{outResultName \code{"character"}}
#'  }
#' @name OutResultReference-class
#' @export
setClass("OutResultReference", representation(outResultName = "character", "VIRTUAL"))

#' @title Accessor getOutResultName
#' @export
#' @docType methods
#' @return outResultName
setGeneric("getOutResultName", function(object){standardGeneric("getOutResultName")})
setMethod("getOutResultName",signature(object="OutResultReference"),function(object) {
  slot(object, "outResultName")
})


#'@title FilesOutput
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'  }
#' @name FilesOutput-class
#' @export
setClass("FilesOutput", contains = "OutResultReference")
#'@title FoldersOutput
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'  }
#' @name FoldersOutput-class
#' @export
setClass("FoldersOutput", contains = "OutResultReference")

#' @title Constructor method for FoldersOutput
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{"FoldersOutput"}
FoldersOutput = function(outResultName){
  return( new("FoldersOutput", outResultName=outResultName) )
}
#' @title Constructor method for FilesOutput
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{"FilesOutput"}
FilesOutput = function(outResultName){
  return( new("FilesOutput", outResultName=outResultName) )
}

#' @title Constructor (factory) for OutResultReference
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{OutResultReference} implementation
OutResultReference = function(outResultName){
  if( file.exists(outResultName) ){
    if( length(list.files(outResultName)) == 0 ){
      return( FilesOutput(outResultName)  )
    } else{
      return( FoldersOutput(outResultName)  )
    }
  } else{
    stop("Not a file or folder!")
  } 
} 

#'@title CmdGenResult
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{CLIApplication \code{"CLIApplication"}}
#'    \item{\code{slot2}:}{OutResultReference \code{"OutResultReference"}}
#'    \item{\code{slot3}:}{commands \code{"character"}}
#'  }
#' @name CmdGenResult-class
#' @export
setClass( "CmdGenResult", representation( CLIApplication="CLIApplication",
                                          OutResultReference="OutResultReference",
                                          commands="character" ) )

setValidity ("CmdGenResult",
             function ( object ){
               retval = NULL
               if ( is.null(retval)){
                 return(TRUE)
               }
               else{
                 return(retval)
               }
             })

#' @title Accessor CLIApplication
#' @export
#' @docType methods
#' @return CLIApplication
setGeneric("getCLIApplication", function(object) standardGeneric("getCLIApplication"))
setMethod("getCLIApplication",signature(object="CmdGenResult"),function(object) {
  slot(object, "CLIApplication")
})

#' @title Accessor getOutResultReference
#' @export
#' @docType methods
#' @return OutResultReference
setGeneric("getOutResultReference", function(object) standardGeneric("getOutResultReference"))
setMethod("getOutResultReference",signature(object="CmdGenResult"),function(object) {
  slot(object, "OutResultReference")
})

#' @title Accessor getCommands
#' @export
#' @docType methods
#' @return commands
setGeneric("getCommands", function(object) standardGeneric("getCommands"))
setMethod("getCommands",signature(object="CmdGenResult"),function(object) {
  slot(object, "commands")
})

#' @title Accessor getCommandLog
#' @export
#' @docType methods
#' @return commands formatted
setGeneric("getCommandLog", function(object) standardGeneric("getCommandLog"))
setMethod("getCommandLog",signature(object="CmdGenResult"),function(object) {
  paste0( getCommands(object),collapse="\n")
})

#' @title Constructor method for CmdGenResult
#' @param CLIApplication
#' @param OutResultReference
#' @param commands
#' @export
#' @docType methods
#' @return \code{"CmdGenResult"}
CmdGenResult = function(CLIApplication,OutResultReference,commands ){
  return( new("CmdGenResult",CLIApplication=CLIApplication,OutResultReference=OutResultReference,commands=commands) )
}

#' @title Executes the commands of a executeCommandResult object
#' @param \code{"CmdGenResult"}
#' @export
#' @docType methods
setGeneric("executeCommandResult", function( object, testing ) { 
  
  if(isClass(object,"CmdGenResult") & missing(testing)){
    executeCommandResult(object=object, testing=FALSE)
  } else if(isClass(object,"CmdGenResult")) {
    executeCommandResult(object=object, testing=testing)
  } else{
    stop( paste("Function for class",class(object), "not defined!"))
  }
})

setMethod("executeCommandResult",signature(object="CmdGenResult", testing="logical"), function(object, testing=FALSE) {
#   message( paste0("Executing command:\n", getCommandLog(object)) )
    logs = lapply( getCommands(object), function(x){
      #setting the directory to the InputFilePath
      currWD = getwd()
      message( paste0("Executing command:\n", x) )
      log = tryCatch({
        if(!testing){
          setwd(getInFilePath(getCLIApplication(object = object)))
          system(x, intern = TRUE)
        } else{
          message("Testing ... command is not executed!")
          return(x)
        }
      }, warning = function(w){
        warning(w)
      }, error = function(e){
        paste0("Error when executing code ", e)
      }, finally = {
        setwd( currWD )
      })
      return(log)
  })
  return( CmdGenResultExec(cmdGenResult=object, execLog=logs))
})


#'@title CmdGenResultExec
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{cmdGenResult \code{"CmdGenResult"}}
#'    \item{\code{slot2}:}{execLog \code{"execLog"}}
#'  }
#' @name CmdGenResultExec-class
#' @export
setClass( "CmdGenResultExec", representation(cmdGenResult="CmdGenResult", execLog="list" ) )


#' @title Constructor method for CmdGenResultExec
#' @param CmdGenResult
#' @param execLog
#' @export
#' @docType methods
#' @return \code{"CmdGenResultExec"}
CmdGenResultExec = function(cmdGenResult, execLog){
  return( new("CmdGenResultExec",cmdGenResult=cmdGenResult,execLog=execLog) )
}

#' @title Accessor getCmdGenResult
#' @export
#' @docType methods
#' @return getCmdGenResult
setGeneric("getCmdGenResult", function(object) standardGeneric("getCmdGenResult"))
setMethod("getCmdGenResult",signature(object="CmdGenResultExec"),function(object) {
  slot(object, "cmdGenResult")
})

#' @title Accessor getExecLog
#' @export
#' @docType methods
#' @return execLog
setGeneric("getExecLog", function(object) standardGeneric("getExecLog"))
setMethod("getExecLog",signature(object="CmdGenResultExec"),function(object) {
  slot(object, "execLog")
})

#' @title Accessor getExecLogFormatted
#' @export
#' @docType methods
#' @return execLog formatted
setGeneric("getExecLogFormatted", function(object) standardGeneric("getExecLogFormatted"))
setMethod("getExecLogFormatted",signature(object="CmdGenResultExec"),function(object) {
  tmp = lapply( getExecLog(object), function(x){
    paste0(x,collapse="\n") 
  } )
  return(  paste0(unlist(tmp) ,collapse="\n\n") )
})






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
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(file.path(getOutFilePath(object),outFN)), commands = c(cmd1, cmd2))
  
  return(res)
  
})




#'@title MultiBamCov_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{annotationFileMB \code{"character"}}
#'    \item{\code{slot7}:}{annotationType \code{"character"}}
#'  }
#' @name MultiBamCov_CLI-class
#' @export
setClass("MultiBamCov_CLI", contains = "CLIApplication", representation(annotationFileMB="character", annotationType = "character"))

#' @title Accessor annotationFileMB
#' @export
#' @docType methods
#' @return annotationFileMB
setGeneric("getAnnotationFileMB", function(object){standardGeneric("getAnnotationFileMB")})
setMethod("getAnnotationFileMB",signature(object="MultiBamCov_CLI"),function(object) {
  slot(object, "annotationFileMB")
})
#' @title Accessor annotationType
#' @export
#' @docType methods
#' @return annotationType
setGeneric("getAnnotationType", function(object){standardGeneric("getAnnotationType")})
setMethod("getAnnotationType",signature(object="MultiBamCov_CLI"),function(object) {
  slot(object, "annotationType")
})

#' @title Constructor method for MultiBamCov_CLI
#' @export
#' @docType methods
setGeneric("MultiBamCov_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB, annotationType){standardGeneric("MultiBamCov_CLI")})
setMethod("MultiBamCov_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                      outputFlag="character", outFilePath="character", annotationFileMB="character", annotationType="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB,annotationType ){
            return(
              new("MultiBamCov_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, annotationFileMB=annotationFileMB,annotationType=annotationType)
            )
          })

setMethod("MultiBamCov_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="character", 
                                       outputFlag="character", outFilePath="character", annotationFileMB="character", annotationType="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, annotationFileMB,annotationType ){
            inFileNames = list.files(path = inFilePath, pattern = ".*\\.sam$")
            return(
              new("MultiBamCov_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, annotationFileMB=annotationFileMB,annotationType=annotationType)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of MultiBamCov_CLI
#' 
#' @param MultiBamCov_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="MultiBamCov_CLI"),function(object) { 
  #   Usage: bedtools multicov [OPTIONS] -bams aln.1.bam aln.2.bam ... aln.n.bam -bed <bed/gff/vcf>
  
  inFNs = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
  outFN = paste0( sub( "\\..*$","",inFNs[1]), getOutputFlag(object) )
  cmd2 = paste0( "bedtools multicov ", paste0(getCliParams(object),collapse=" "), " -bams ", paste0(inFNs, collapse=" "), " -",
                 getAnnotationType(object)," ", getAnnotationFileMB(object), " > ",  file.path(getOutFilePath(object),outFN)  )
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(file.path(getOutFilePath(object),outFN)), commands = c(cmd1, cmd2))
  
  return(res)
  
})



#'@title Samtools_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{samtoolsApplication \code{"character"}}
#'    \item{\code{slot6}:}{outputFormat \code{"character"}}
#'  }
#' @name Samtools_CLI-class
#' @export
setClass("Samtools_CLI", contains = "CLIApplication", representation(samtoolsApplication="character", outputFormat = "character") )

# @title Validity method for Samtools_CLI
# @export
# @name validitySamtools_CLI
# @rdname validitySamtools_CLI
# @docType methods
setValidity ("Samtools_CLI",
             function ( object ){
               retval = NULL
               
               if( length(getInFileNames(object)) > 1 ){ retval = "Only one input file is allowed!" }         
               
               if ( is.null(retval)){
                 return(TRUE)
               }
               else{
                 return(retval)
               }
             })

#' @title Accessor getSamtoolsApplication 
#' @export
#' @docType methods
#' @return samtoolsApplication
setGeneric("getSamtoolsApplication", function(object){standardGeneric("getSamtoolsApplication")})
setMethod("getSamtoolsApplication", signature(object="Samtools_CLI"), function(object){
  slot(object, "samtoolsApplication")
})

#' @title Accessor getOutputFormat
#' @export
#' @docType methods
#' @return outputFormat
setGeneric("getOutputFormat", function(object){standardGeneric("getOutputFormat")})
setMethod("getOutputFormat", signature(object="Samtools_CLI"), function(object){
  slot(object, "outputFormat")
})

#' @title Constructor method for Samtools_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("Samtools_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, samtoolsApplication, outputFormat){standardGeneric("Samtools_CLI")})
setMethod("Samtools_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", samtoolsApplication="character", outputFormat="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, samtoolsApplication, outputFormat){
            return(
              new("Samtools_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, samtoolsApplication=samtoolsApplication, outputFormat=outputFormat)
            )
          })


#' @title Generates the commands and possibly resulting files/folders of Samtools
#' 
#' @param Samtools_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Samtools_CLI"),function(object) { 
  
  cmd1 = paste0("cd ",getInFilePath(object))
  
  #select different samtools applications by name
  stapp = getSamtoolsApplication(object)
  inFN = getInFileNames(object)
  outfmt = getOutputFormat(object)
  outFN = ""
  cmd2 = ""
  switch(stapp, 
         view = {
           #samtools view -o aligned_sn.sam aligned_sn.bam
           outFN = paste0( sub("\\..*$","",inFN),getOutputFlag(object),".",outfmt)
           
           params = getCliParams(object)
           params = params[ which( params != "-o" ) ]
           
           cmd2 = paste0("samtools view ",paste0(params,collapse=" ")," -o ",outFN," ", inFN)
         },
         sort = {
           #samtools sort -n aligned.bam aligned_sn
           outFNprefix = paste0( sub("\\..*$","",inFN),getOutputFlag(object) )
           outFN = paste0(outFNprefix, ".", outfmt)
           cmd2 = paste0("samtools sort ",paste0(getCliParams(object),collapse=" ")," ", inFN," ", outFNprefix )
           
          },
         index = {
           #samtools index aligned_s.bam
           cmd2 = paste0("samtools index ",paste0(getCliParams(object),collapse=" ")," ",inFN)
           outFN = sub( "bam","bai", inFN)
         }, 
         {
         stop("Samtools application not recognized (supported: view, sort, index)")
         })
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(file.path(getOutFilePath(object),outFN)), commands = c(cmd1, cmd2))
  
  return(res)

})


#'@title Tophat2_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{bowtieIndexFilePath \code{"character"}}
#'  }
#' @name Tophat2_CLI-class
#' @export
setClass("Tophat2_CLI", contains = "CLIApplication", representation(bowtieIndexFilePath="character") )

#' @title Accessor getBowtieIndexFilePath Tophat2
#' @export
#' @docType methods
#' @return bowtieIndexFilePath
setMethod("getBowtieIndexFilePath",signature(object="Tophat2_CLI"),function(object) {
  slot(object, "bowtieIndexFilePath")
})

#' @title Constructor method for Tophat2_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("Tophat2_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath,bowtieIndexFilePath){standardGeneric("Tophat2_CLI")})
setMethod("Tophat2_CLI", signature( inFilePath="character", inFileNames="missing", cliParams="character", 
                                    outputFlag="character", outFilePath="character",  
                                    bowtieIndexFilePath="character"), 
                                    function(inFilePath, cliParams, outputFlag, outFilePath, bowtieIndexFilePath){
                                      inFileNames = list.files(path = inFilePath, pattern = ".*\\.fastq$")
                                      return(
                                        new("Tophat2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                                            outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath)
                                      )
                                    })
setMethod("Tophat2_CLI", signature( inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character",  
                                    bowtieIndexFilePath="character"), 
          function(inFilePath,inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath){
            return(
              new("Tophat2_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of Tophat2
#' 
#' @param Tophat2_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Tophat2_CLI"),function(object) { 
  
#   USAGE:
#   tophat [options] <bowtie_index> <reads1[,reads2,...]> [reads1[,reads2,...]] \
#   [quals1,[quals2,...]] [quals1[,quals2,...]]

  
  #preparing input files 
  inFP = getInFilePath(object)
  inFN = getInFileNames(object)
  
  #preparing output files
  sampleOutDir = paste0( paste0( sub("\\.fastq$","",inFN),getOutputFlag(object))  )
  sampleOutDir = file.path(getOutFilePath(object), sampleOutDir)
  #generating the tophat base directory
  outfilepath = getOutFilePath(object)
#   dir.create(outfilepath)
#   setwd(outfilepath)
  
  cmd1 = paste("mkdir", outfilepath) #if only executed in terminal!
  cmd2 = paste("cd ", outfilepath)
  
  #generating the tophat command
  cmd3 = paste0( "tophat2 ", paste0(getCliParams(object), collapse=" "), 
                 " --output-dir ", sampleOutDir, " ", getBowtieIndexFilePath(object), " ", file.path(inFP,inFN) )
  
  #IN FILENAME UND BOWTIE INDEX PATH FEHLEN!
  
  #   tophat2 --num-threads 12 --keep-fasta-order --output-dir /home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/tophat-out_sampleAnalysis --GTF /home/simon/dbsOfflineUse/GTF_repos/Mus_musculus.GRCm38.78_mod.gtf /home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10 /home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/sampleFastq_larger20.fastq

  res = CmdGenResult(CLIApplication = object, OutResultReference = FoldersOutput(sampleOutDir), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})






