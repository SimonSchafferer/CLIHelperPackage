#--split CmdGenResultExec

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




