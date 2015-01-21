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


