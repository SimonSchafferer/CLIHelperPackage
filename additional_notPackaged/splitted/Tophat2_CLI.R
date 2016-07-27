#--split Tophat2_CLI

#@include CLIApplication
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
  
  #generating the tophat base directory
  outfilepath = getOutFilePath(object)
#   dir.create(outfilepath)
#   setwd(outfilepath)
  
  cmd1 = paste("mkdir", outfilepath) #if only executed in terminal!
  cmd2 = paste("cd ", outfilepath)
  
  #generating the tophat command
  cmd3 = paste0( "tophat2 ", paste0(getCliParams(object), collapse=" "), 
                 " --output-dir ", file.path(file.path(getOutFilePath(object), sampleOutDir)), " ", getBowtieIndexFilePath(object), " ", file.path(inFP,inFN) )
  
  #IN FILENAME UND BOWTIE INDEX PATH FEHLEN!
  
  #   tophat2 --num-threads 12 --keep-fasta-order --output-dir /home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/tophat-out_sampleAnalysis --GTF /home/simon/dbsOfflineUse/GTF_repos/Mus_musculus.GRCm38.78_mod.gtf /home/simon/dbsOfflineUse/MusMusculus/Mouse_mm10_fasta/bowtie2/mm10 /home/simon/PHDStudies/RNA-Seq/IonProton/MSA_LusserWenning/SampleAnalysis/sampleFastq_larger20.fastq

  res = CmdGenResult(CLIApplication = object, OutResultReference = FoldersOutput(sampleOutDir), commands = c(cmd1, cmd2, cmd3))
  
  return(res)
})






