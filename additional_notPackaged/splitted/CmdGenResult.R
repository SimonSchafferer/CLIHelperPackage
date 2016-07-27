#--split CmdGenResult

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
  paste0(paste0( getCommands(object),collapse="\n"), "\n")
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
setGeneric("executeCommandResult", function(object, ...) {
  standardGeneric("executeCommandResult") })
#' @title Executes the commands of a executeCommandResult object
#' @param \code{"CmdGenResult"}
#' @param \code{"testing"} boolean testing if true - command is not executed
#' @export
#' @docType methods
setMethod("executeCommandResult",signature(object="CmdGenResult"), function(object, testing=FALSE, useTee=TRUE) {

  #   message( paste0("Executing command:\n", getCommandLog(object)) )
  logs = lapply( getCommands(object), function(x){
    #setting the directory to the InputFilePath
    currWD = getwd()
    message( paste0("Executing command:\n", x) )
    log = tryCatch({
      if(!testing){
        if(!useTee){
          setwd(getInFilePath(getCLIApplication(object = object)))
          cmdRes = system(x,intern = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE)
          return(cmdRes)
        } else{
          setwd(getInFilePath(getCLIApplication(object = object)))
          logFile = tempfile()
          system(paste0(x," 2>&1 | tee ",logFile),intern = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE)
          return( readLines(logFile) )
        }
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

