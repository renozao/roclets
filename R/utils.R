# Utils functions
# 
# Author: Renaud Gaujoux
###############################################################################

`%||%` <- function(a, b) if( is.null(a) ) b else a

#' @import bibtex
# copied and fixed from bibtex::read.bib
read.bib2 <- function (file = findBibFile(package), package = "bibtex", encoding = "unknown", 
    header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
    footer = "") 
{
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
          encoding = encoding))
  out <- .External("do_read_bib", file = file, encoding = encoding, 
      srcfile = srcfile)
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, make.bib.entry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- make.citation.list(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  # keys must be retrieved here to handle the case of skipped bibentries
  `%||%` <- function(a, b) if( is.null(a) ) b else a
  keys <- sapply(out, function(x) attr(x, 'key') %||% x$key)
  names(out) <- keys
  out
}
environment(read.bib2) <- asNamespace('bibtex')


block_tags <- function(x, tag) {
  x[names(x) %in% tag]
}

block_object <- function(x){
  x[['object']] %||% attr(x, 'object')
  
}

# fix block for backward compatibility
block_backport <- function(block){
  if( is.null(block) ) return(block)
  
  block$object <- block_object(block)
  if( !length(attr(block, 'filename')) ) attr(block, 'filename') <- ''
  if( !length(attr(block, 'location')) ) attr(block, 'location') <- ''
  
  block
  
}
