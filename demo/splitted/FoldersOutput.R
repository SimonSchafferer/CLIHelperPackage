#--split FoldersOutput

#'@include OutResultReference
#'@title FoldersOutput
#'@section Slots: 
#'  \describe{
#'    \item{\code{slot1}:}{inFilePath \code{"character"}}
#'  }
#' @name FoldersOutput-class
#' @export
setClass("FoldersOutput", contains = "OutResultReference")

#' @title Constructor method for FoldersOutput
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{"FoldersOutput"}
FoldersOutput = function(outResultName){
  return( new("FoldersOutput", outResultName=outResultName) )
}
#' @title Constructor method for FilesOutput
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{"FilesOutput"}
FilesOutput = function(outResultName){
  return( new("FilesOutput", outResultName=outResultName) )
}

#' @title Constructor (factory) for OutResultReference
#' @param outResultName
#' @export
#' @docType methods
#' @return \code{OutResultReference} implementation
OutResultReference = function(outResultName){
  if( file.exists(outResultName) ){
    if( length(list.files(outResultName)) == 0 ){
      return( FilesOutput(outResultName)  )
    } else{
      return( FoldersOutput(outResultName)  )
    }
  } else{
    stop("Not a file or folder!")
  } 
} 

