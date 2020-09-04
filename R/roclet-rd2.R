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
  
  # Must start by processing templates
  blocks <- lapply(blocks, roxygen2:::process_templates, base_path = base_path)
  
  blocks <- .add_internal_blocks(blocks, env, base_path)
  blocks <- .add_method_mini_sections(blocks, env, base_path)
  blocks <- .process_describeIn_into_minidesc(blocks, env, base_path)
  blocks <- .process_references(blocks, env, base_path)
  
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
