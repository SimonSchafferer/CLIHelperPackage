#--split Bowtie2TophatIon_CLI

#@include Bowtie2_CLI
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
#'    \item{\code{slot9}:}{addNHTag \code{"character"}}
#'  }
#' @name Bowtie2TophatIon_CLI-class
#' @export
setClass("Bowtie2TophatIon_CLI", contains = "Bowtie2_CLI", representation(outFileNames="character", pathToPicardTools="character", addNHTag="logical"), 
         prototype(outFileNames = "unmapped"))

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
setGeneric("Bowtie2TophatIon_CLI", function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames, 
                                            outFileNames, pathToPicardTools, addNHTag){standardGeneric("Bowtie2TophatIon_CLI")})

setMethod("Bowtie2TophatIon_CLI", signature(inFilePath="character", inFileNames="missing", cliParams="missing", 
                                   outputFlag="character", outFilePath="missing", bowtieIndexFilePath="character", matepairFileNames="missing", 
                                   outFileNames="missing", pathToPicardTools="missing", addNHTag="logical"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames,outFileNames, pathToPicardTools, addNHTag){
                            
            warning("Options are locked and input file name is set to unmapped, outfile is set to bam mate pairs are not supported!")
            if(missing(outFileNames)){outFileNames = "aligned"}
            if(missing(pathToPicardTools)){pathToPicardTools = "/usr/local/applications/picard-tools-1.77/"}
            if(missing(outFilePath)){outFilePath = inFilePath; warning("outFilePath is set to inFilePath")}
            
            return(
              new("Bowtie2TophatIon_CLI", inFilePath=inFilePath, inFileNames="unmapped", cliParams=c("--local","--very-sensitive-local","-p 8","--mm"), 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames="", 
                  outFileNames = outFileNames, pathToPicardTools=pathToPicardTools, addNHTag = addNHTag)
            )
          })


setMethod("Bowtie2TophatIon_CLI", signature(inFilePath="character", inFileNames="character", cliParams="character", 
                                            outputFlag="character", outFilePath="character", bowtieIndexFilePath="character", matepairFileNames="character", 
                                            outFileNames="character", pathToPicardTools="character", addNHTag="logical"), 
          function(inFilePath, inFileNames, cliParams, outputFlag, outFilePath, bowtieIndexFilePath,matepairFileNames,outFileNames, pathToPicardTools, addNHTag){
            return(
              new("Bowtie2TophatIon_CLI", inFilePath=inFilePath, inFileNames=inFileNames, cliParams=cliParams, 
                  outputFlag=outputFlag, outFilePath=outFilePath, bowtieIndexFilePath=bowtieIndexFilePath, matepairFileNames=matepairFileNames, 
                  outFileNames = outFileNames, pathToPicardTools=pathToPicardTools, addNHTag = addNHTag)
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


#' @title Accessor getAddNHTag
#' @export
#' @docType methods
#' @return addNHTag
setGeneric("getAddNHTag", function(object){standardGeneric("getAddNHTag")})
setMethod("getAddNHTag",signature(object="Bowtie2_CLI"),function(object) {
  slot(object, "addNHTag")
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
    
    if( getAddNHTag(object) ){
      cmd3 = c(cmd3, paste0("samtools sort -n unmapped_remap.bam unmapped_remap_sn") )
      cmd3 = c(cmd3, paste0( "python -c \"import bbcflib.mapseq; bbcflib.mapseq.add_nh_flag('",file.path( inFP,"unmapped_remap_sn.bam"),
                            "', '",file.path( inFP,"unmapped_remap.bam"),"')\"" ) )    
    }
  
    cmd4 = paste0("java -jar ", file.path( getPathToPicardTools(object), "MergeSamFiles.jar"),
                  " USE_THREADING=true MSD=true AS=true I=accepted_hits.bam I=unmapped_remap.bam O=",file.path(getOutFilePath(object), outFile) )
        
    res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFile), commands = c(cmd1, cmd2, cmd3, cmd4))
    return(res)
  } )


