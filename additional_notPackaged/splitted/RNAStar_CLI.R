#--split RNAStar_CLI

#@include CLIApplication
#'@title RNAStar_CLI
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'    \item{\code{slot2}:}{inFileNames \code{"character"}}
#'    \item{\code{slot3}:}{cliParams \code{"character"}}
#'    \item{\code{slot4}:}{outFilePath \code{"character"}}
#'    \item{\code{slot5}:}{outputFlag \code{"character"}}
#'    \item{\code{slot6}:}{genomeIndexFilePath \code{"character"}}
#'    \item{\code{slot7}:}{filterSam \code{"logical"}}
#'    \item{\code{slot8}:}{outputFormat \code{"character"}}
#'  }
#' @name RNAStar_CLI-class
#' @export
setClass("RNAStar_CLI", contains = "CLIApplication", representation(genomeIndexFilePath="character", filterSam="logical", outputFormat="character") )

#' @title Accessor genomeIndexFilePath
#' @export
#' @docType methods
#' @return genomeIndexFilePath
setGeneric("getGenomeIndexFilePath", function(object) standardGeneric("getGenomeIndexFilePath"))
setMethod("getGenomeIndexFilePath",signature(object="RNAStar_CLI"),function(object) {
  slot(object, "genomeIndexFilePath")
})

#' @title Accessor getOutputFormat already defined in Samtools
#' @export
#' @docType methods
#' @return outputFormat
setMethod("getOutputFormat",signature(object="RNAStar_CLI"),function(object) {
  slot(object, "outputFormat")
})


#' @title Accessor filterSam
#' @export
#' @docType methods
#' @return getFilterSam
setGeneric("getFilterSam", function(object) standardGeneric("getFilterSam"))
setMethod("getFilterSam",signature(object="RNAStar_CLI"),function(object) {
  slot(object, "filterSam")
})

#' @title Constructor method for RNAStar_CLI
#' @param inFilePath (inFileNames may also be specified otherwise they will be fetched by list.files)
#' @export
#' @docType methods
setGeneric("RNAStar_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath,genomeIndexFilePath,filterSam, outputFormat){standardGeneric("RNAStar_CLI")})
setMethod("RNAStar_CLI", signature( inFilePath="character", inFileNames="character", cliParams="character", 
                                    outputFlag="character", outFilePath="character",  
                                    genomeIndexFilePath="character", filterSam="logical", outputFormat="character"), 
          function(inFilePath,inFileNames, cliParams, outputFlag, outFilePath, genomeIndexFilePath,filterSam, outputFormat){
            return(
              new("RNAStar_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, genomeIndexFilePath=genomeIndexFilePath,filterSam=filterSam, outputFormat=outputFormat)
            )
          })

#' @title Generates the commands and possibly resulting files/folders of Tophat2
#' 
#' @param RNAStar_CLI
#' @return CmdGenResult
#' @rdname generateCommandResult-method
#' @export
setMethod("generateCommandResult",signature(object="RNAStar_CLI"),function(object) { 
  
  #   USAGE:
  #  STAR --genomeDir <Directory with the Genome Index>  --runThreadN <# cpus> --readFilesIn <FASTQ file> --outFileNamePrefix <OutputPrefix>
  if(length(getInFileNames(object)) > 1){
    stop("Only one fasta file per call, if more calls please use option --genomeLoad LoadAndKeep which will keep the index file in the memory")
  }
  
  #preparing input files 
  inFP = getInFilePath(object)
  inFN = getInFileNames(object)
  
  #preparing output files
  sampleOutDir = paste0( paste0( sub("\\.fastq$","",inFN),getOutputFlag(object))  )
  
  #generating the tophat base directory
  outfilepath = getOutFilePath(object)
  
  if( getOutputFormat(object) == "bam" ){
    outFileName = paste0(getOutputFlag(object), "Aligned.out.bam")
  } else{
    outFileName = paste0(getOutputFlag(object), "Aligned.out.sam")
    outFileName_filt = paste0(getOutputFlag(object), "Aligned.out.filtered.sam")
    
  }
  
#   dir.create(outfilepath)
#   setwd(outfilepath)
  
  cmd1 = paste("mkdir", outfilepath) #if only executed in terminal!
  cmd2 = paste("cd ", outfilepath)
  
  #generating the tophat command
  cmd3 = paste0( "STAR --genomeDir ",getGenomeIndexFilePath(object)," --readFilesIn ",file.path(inFP,inFN), 
                 " --outFileNamePrefix ",getOutputFlag(object), " ", paste0(getCliParams(object), collapse=" ") )
  #" --runThreadN 6 --outFilterMismatchNoverLmax 0.05 --outFilterMatchNmin 16 --outFilterScoreMinOverLread 0  --outFilterMatchNminOverLread 0 --alignIntronMax 1 --clip3pAdapterSeq ATCACCGACTGCCCATAGAGAGGCTGAGAC --clip3pAdapterMMp 0.1 --outFilterMultimapNmax 100"
    #(>=16b matched to the genome, number of mismatches <= 5% of mapped length, i.e. 0MM for 16-19b, 1MM for 20-39b etc, splicing switched off). will also trim by IonProton 3pAdapter obtained from Martina. If more than 100 matches in the genome - filter out reads!
  
  #After running, filtering: awk script will filter out all alignments that are trimmed by more than 1 base from the 5' end! 
  cmd4 = ""
  if(getFilterSam(object) & getOutputFormat(object) != "bam"){
    cmd4 = paste("cd ", outfilepath)
    cmd5 = paste0( "awk '{S=0; split($6,C,/[0-9]*/); n=split($6,L,/[NMSID]/);  
                   if (and($2,0x10)>0 && C[n]==\"S\") {S=L[n-1]} else if (and($2,0x10)==0 && C[2]==\"S\") {S=L[1]}; if (S<=1) print }' ",outFileName," > ",outFileName_filt  )  
    cmd6 = paste0( "rm ",outFileName )#removing the unfiltered file
    cmd7 = paste0( "mv ", outFileName_filt, " ", outFileName) #renaming the file
    cmd3 = c(cmd3, cmd4, cmd5, cmd6, cmd7)
  }
  
  if( getOutputFormat(object) != "bam" ){  
    outBamFN = paste0(sub(".sam$","",outFileName ),".bam")
    #Writing sam to bam file
    cmd4 = paste0( "samtools view -Sb ",outFileName," > ",outBamFN)
    cmd4 = c(cmd4, paste0("rm ",outFileName) )
  } else{
    outBamFN = outFileName
  }
    
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outBamFN) , commands = c(cmd1, cmd2, cmd3, cmd4))
  
  return(res)
})





