# Roxygen extensions
# 
# Author: Renaud Gaujoux
###############################################################################

#' Replacement Roclet for Generating Rd Files
#' 
#' This roclets acts as a drop-in replacement for [roxygen2::rd_roclet], 
#' adding some extra features such as bibliography, or inline declaration
#' for S4 generic.
#' 
#' To use it add it needs to be declared in the roxygen options within 
#' your package's DESCRIPTION file:
#' ```
#' Roxygen: list(roclets = c('collate', 'namespace', 'roclets::rd2_roclet'))
#' ```
#' 
#' @details
#' Extra features are sometimes submitted as pull request to be 
#' incorporated into the main `roxygen2` package.
#' This roclet enables using them while they are being reviewed -- and 
#' potentially not accepted.
#' 
#' @section Bibliography-backed references:
#' 
#' Adds support for the following:
#' 
#'   * tag `@@bibliography` in roxygen chunks: declares external Bibtex files in which the entries  
#'   parsed from `@@cite`, `\\cite`, `\\citep` or `[@@]` commands are copied from. This is typically a large
#'   file that contains all your references.
#'   * tag `@@cite <bibtex_entries>` in roxygen chunks: to add references to the man page using their
#'   bibtex entry.
#'   * `\\cite` or `\\citep` in Latex markup-based documents such as vignettes or 
#'   roxygen chunks
#'   * `[@@<author2019>]` in markdown-based documents such as vignettes 
#'   or roxygen chunks
#'   
#' At the end of a `roxygenize` run, all the references found in the package man pages
#' or vignettes are gathered and stored in file `inst/REFERENCES.bib` in the source 
#' package directory, hence making the generation of package independent of the -- large --
#' external Bibtex file.
#' 
#' @noMd
#' @import roxygen2 digest
#' @export
#' @examples 
#' library(roxygen2)
#' text <-"
#' #' Title
#' #'
#' #' @bibliography a/b/c.bib
#' f <- function(){
#' }
#' "
#' parse_text(text)
#' 
rd2_roclet <- function() {
  roc <- roclet("rd2")
  # Add roclet_rd class to inherit its methods
  class(roc) <- append(class(roc), 'roclet_rd', after = 1L)
  roc
  
}

#' @inheritParams roxygen2::roclet_process
#' @importFrom stats setNames
#' @export
#' @rdname rd2_roclet
roclet_process.roclet_rd2 <- function(x, blocks, env, base_path = env){
  
  parsed <- if( !is.null(blocks[['blocks']]) ) blocks else list(blocks = blocks) 
  parsed$env <- parsed$env %||% env
  # build inline set
  inline_set <- unlist(sapply(parsed$blocks, function(x){
        x <- block_backport(x)
        if( !is.null(x$inline) && class(x$object) == 's4generic' ){
          setNames(as.character(x$object$value@generic), x$rdname)
        }
    }))
  
  # get bibfile cache object
  BIBS <- RoxyBibObject(base_path = base_path)
  # parse citations in vignettes
  process_cite_vignettes(BIBS, base_path)
  
  # extract citations in tag values and add them as reference tags
  for (i in seq_along(parsed$blocks)) {
    block <- parsed$blocks[[i]]
    hash <- digest(block)
    block <- block_backport(block)
    
    # NB: for describeIn S4 classes, the destination tag should be the name of the class, 
    # not <name>-class
    if( any(class(block$object) %in% 's4method') ){
        generic_name <- as.character(block$object$value@generic) 
        if( (generic_name %||% '') %in% inline_set && is.null(block$describeIn) ){
#          i <- match(generic_name, inline_set)
#          target_name <- names(inline_set)[i]
#          str(target_name)
#          if( is.na(target_name) || target_name == '' ) target_name <- generic_name
          target_name <- generic_name
          # build inline description from title and description
          descIn <- list(name = target_name
                          , description = paste(block$title, if( !identical(block$description, block$title) ) block$description, sep = "\n\n"))
          if( !length(descIn$description) ) block$rdname <- block$rdname %||% descIn$name 
          else{
            block$describeIn <- descIn
            block$title <- block$description <- NULL
        }
      }
    }
    
    if (length(block) == 0)
      next
    
    # process cite
    block <- process_cite(block, base_path, parsed$env)
    
    if( digest(block) != hash ) parsed$blocks[[i]] <- block
  }
  
  blocks <- parsed[['blocks']]
  # call roclet_rd process method to update the .Rd files
  NextMethod()
  
}

#' @inheritParams roxygen2::roclet_output
#' @export
roclet_output.roclet_rd2 <- function(x, results, base_path, ..., is_first = FALSE) {
  
  # update REFERENCES file
  RoxyBibObject()$update_bibfile(file.path(base_path, 'inst/REFERENCES.bib'))
  
  NextMethod()
}


#' @export
#' @rdname rd2_roclet
roclet_clean.roclet_rd2 <- function(x, base_path) {
  # reset bibliography object
  RoxyBibObject(reset = TRUE)
  # call roclet_rd clean method
  NextMethod()
}
