#--split OutResultReference

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

