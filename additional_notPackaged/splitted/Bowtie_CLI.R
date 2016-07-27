#--split Bowtie_CLI

#################################################
#     General generic function definitions that are implemented in several subclasses! 
#     MUST BE PLACED AFTER Bowtie2TophatIon_CLI class!
#################################################

#@include Samtools_CLI,Bowtie2Tophation_CLI
#'@title Bowtie_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{bowtieIndexFilePath \code{"character"}}
#'    \item{\code{slot7}:}{matepairFileNames \code{"character"}}
#'    \item{\code{slot8}:}{addNHTag \code{"logical"}}
#'    \item{\code{slot9}:}{outputPipeString \code{"character"}}
#'    \item{\code{slot10}:}{outputFormat \code{"character"}}
#'  }
#' @name Bowtie_CLI-class
#' @export
setClass("Bowtie_CLI", contains = "CLIApplication", representation(bowtieIndexFilePath="character", matepairFileNames="character", 
                                                                   addNHTag="logical", outputPipeString="character", outputFormat = "character") )

#' @title Accessor getBowtieIndexFilePath
#' @export
#' @docType methods
#' @return bowtieIndexFilePath
setMethod("getBowtieIndexFilePath",signature(object="Bowtie_CLI"),function(object) {
  slot(object, "bowtieIndexFilePath")
})

#' @title Accessor getMatepairFileNames
#' @export
#' @docType methods
#' @return matepairFileNames
setMethod("getMatepairFileNames",signature(object="Bowtie_CLI"),function(object) {
  slot(object, "matepairFileNames")
})

#' @title Accessor getAddNHTag
#' @export
#' @docType methods
#' @return addNHTag
setMethod("getAddNHTag",signature(object="Bowtie_CLI"),function(object) {
  slot(object, "addNHTag")
})

#' @title Accessor getOutputPipeString
#' @export
#' @docType methods
#' @return outputPipeString
setGeneric("getOutputPipeString", function(object){standardGeneric("getOutputPipeString")})
setMethod("getOutputPipeString",signature(object="Bowtie_CLI"),function(object) {
  slot(object, "outputPipeString")
})

#' @title Accessor getOutputFormat
#' @export
#' @docType methods
#' @return outputFormat
setMethod("getOutputFormat", signature(object="Bowtie_CLI"), function(object){
  slot(object, "outputFormat")
})

#' @title Constructor method for Bowtie_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("Bowtie_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath, matepairFileNames, 
                                  addNHTag, outputPipeString, outputFormat){standardGeneric("Bowtie_CLI")})

setMethod("Bowtie_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", 
                                  matepairFileNames="missing", addNHTag="logical", outputPipeString="character", outputFormat="character"), 
           function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames, addNHTag, outputPipeString, outputFormat ){
             return(
               new("Bowtie_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                   outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames="", 
                   addNHTag=addNHTag, outputPipeString=outputPipeString, outputFormat=outputFormat)
             )
          })
setMethod("Bowtie_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                   outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", 
                                  matepairFileNames="character", addNHTag="logical", outputPipeString="character", outputFormat="character"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames, addNHTag, outputPipeString, outputFormat ){
            return(
              new("Bowtie_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, 
                  matepairFileNames=matepairFileNames, addNHTag=addNHTag, outputPipeString=outputPipeString, outputFormat=outputFormat)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of Bowtie2
#' 
#' @param Bowtie_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="Bowtie_CLI"),function(object) { 
 #USAGE: bowtie [options]* <ebwt> {-1 <m1> -2 <m2> | --12 <r> | <s>} [<hit>]
  
  #bowtie -qSya -n 1 -p 3 --chunkmbs 1024 --best -l 28 -m 100 --phred64-quals --strata /home/simon/dbsOfflineUse/apartdbs/mouse_mm9/genome test_trimmed.fastq | samtools view -uhS -F4 - | samtools sort - /tmp/mapped
  #This command -l 18 nt seed, -n 1 in seed 1 mismatch (seed generated from left), -m maximum 100 alignments reported (not randomly chosen if more -> would be -M 100) 
  #the maximum 100 alignments are chosen from the best stratum of the aligner, -p n number of processor cores, outputPipeString is optional here
  
  inFN = getInFileNames(object)
  cmd1 = paste0("cd ",getInFilePath(object))
  
  #trim the file extension to create the outputFileName
  inFNtrimmed = sub("\\..+$","",inFN)
  outFN = paste0( inFNtrimmed, getOutputFlag(object),".", getOutputFormat(object))
  outFNtrimmed = paste0( inFNtrimmed, getOutputFlag(object))
  
  # create the output directory
  cmd2 = paste0("mkdir ", getOutFilePath(object)) #if only executed in terminal!
  cmd2 = c(cmd2,paste0("cd ",getOutFilePath(object)))
  #Executing bowtie
  inFileNames = paste0( file.path(getInFilePath(object),inFN), collapse=",")
  
  if( getMatepairFileNames(object)[1] == "" ){
    cmd3 = paste0("bowtie ", paste0(getCliParams(object),collapse=" "), " ", getBowtieIndexFilePath(object), " ", inFileNames, " ",
                   " ", getOutputPipeString(object), " ", file.path(getOutFilePath(object), outFNtrimmed) )
  } else{
    cmd3 = paste0("bowtie ", paste0(getCliParams(object),collapse=" "), " ", getBowtieIndexFilePath(object), " -1 ", inFileNames,
                 " -2 ",paste0( getMatepairFileNames(object), collapse=","), " ", getOutputPipeString(object), " ",  file.path(getOutFilePath(object), outFNtrimmed) )
  }
  
  if( getAddNHTag(object) ){
      sortedFN = paste0(file.path(getOutFilePath(object), outFNtrimmed),"_sn")
      
      cmd3 = c(cmd3, paste0("samtools sort -n ",file.path(getOutFilePath(object), outFN), " ", sortedFN  ) )
      cmd3 = c(cmd3, paste0( "python -c \"import bbcflib.mapseq; bbcflib.mapseq.add_nh_flag('",paste0(sortedFN,".",getOutputFormat(object)),"', '",file.path(getOutFilePath(object), outFN),"')\"" ) )
      cmd3 = c(cmd3, paste0("rm ", paste0(sortedFN,".",getOutputFormat(object))  ) )#Another sorting so that the sorted file contains the flag too
    }

  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})

