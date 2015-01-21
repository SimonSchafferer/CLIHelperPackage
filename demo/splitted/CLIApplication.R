#--split CLIApplication

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


