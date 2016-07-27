#--split Samtools_CLI

#@include CLIApplication
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
           cmd2 = paste0("samtools sort ",paste0(getCliParams(object),collapse=" ")," ", inFN," -o ", outFN )
           
          },
         index = {
           #samtools index aligned_s.bam
           cmd2 = paste0("samtools index ",paste0(getCliParams(object),collapse=" ")," ",inFN)
           outFN = sub( "bam","bai", inFN)
         }, 
         {
         stop("Samtools application not recognized (supported: view, sort, index)")
         })
  
  res = CmdGenResult(CLIApplication = object, OutResultReference = FilesOutput(outFN), commands = c(cmd1, cmd2))
  
  return(res)

})

