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
#' This roclet enables using while they are being reviewed -- and 
#' potentially not accepted.
#' 
#' @import roxygen2 digest
#' @export
rd2_roclet <- function() {
  roc <- roclet("rd2")
  # Add roclet_rd class to inherit its methods
  class(roc) <- append(class(roc), 'roclet_rd', after = 1L)
  roc
}

#' @section Roxygen tags and commands:
#'   * @@cite: 
#'   * @@bibliography:
#'   * \\cite: 
#'   * @@inline:
#' 
#' @export
#' @rdname rd2_roclet
roclet_tags.roclet_rd2 <- function(x) {
  # call rd_roclet tags method
  tags <- NextMethod()
  
  # add extra tags
  c(tags, list(
    cite = tag_cite,
    bibliography = tag_bibliography,
    inline = tag_toggle
  ))
  
}

#' @inheritParams roxygen2::roclet_process
#' @export
#' @rdname rd2_roclet
roclet_process.roclet_rd2 <- function(x, parsed, base_path, global_options = list()){
  
  # build inline set
  inline_set <- unlist(sapply(parsed$blocks, function(x){
        if( !is.null(x$inline) && class(x$object) == 's4generic' ) as.character(x$object$value@generic)
    }))
  
  # get bibfile cache object
  BIBS <- RoxyBibObject(base_path = base_path)
  
  # extract citations in tag values and add them as reference tags
  for (i in seq_along(parsed$blocks)) {
    block <- parsed$blocks[[i]]
    hash <- digest(block)
    
    if( class(block$object) == 's4method' && 
        (as.character(block$object$value@generic) %||% '') %in% inline_set && 
        is.null(block$describeIn) ){
      # build inline description from title and description
      descIn <- list(name = as.character(block$object$value@generic)
                      , description = paste(block$title, if( !identical(block$description, block$title) ) block$description, sep = "\n\n"))
      if( !length(descIn$description) ) block$rdname <- block$rdname %||% descIn$name 
      else{
        block$describeIn <- descIn
        block$title <- block$description <- NULL
      }
    }
    
    if (length(block) == 0)
      next
    
    # process cite
    block <- process_cite(block, base_path, parsed$env, global_options)
    
    if( digest(block) != hash ) parsed$blocks[[i]] <- block
  }
  
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
