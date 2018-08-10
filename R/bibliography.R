# Bibliography support
#
# Author: Renaud Gaujoux
###############################################################################

## Handling of bibliography-related tags
#' Extra Roxygen Tags
#'
#' @param x roxy_tag object
#'
#' @export
#' @rdname roxy_tag
tag_cite <- function(x){
  # convert into @references \cite{<entry1>} \cite{<entry2>} ...
  x <- tag_words()(x)
  roxy_tag('references', val = sprintf("\\cite{%s}", x$val))
}

#' @export
#' @rdname roxy_tag
tag_bibliography <- function(x){
  # add path to bib file to current BibObject handler
  bib <- RoxyBibObject()
  x <- tag_words()(x)
  x <- block_backport(x)
  bib$add_bibfile(x$val, block = x)
  NULL
}

# parse vignettes for \cite commands
process_cite_vignettes <- function(roxybib, base_path){

  # list vignettes
  vfiles <- list.files(file.path(base_path, 'vignettes'), pattern = "\\.((rmd)|(rnw))$", ignore.case = TRUE, full.names = TRUE)
  # parse and format to add to current roxy bib object
  lapply(vfiles, function(f){
        l <- readLines(f)
        i <- grep("^\\s*\\\\begin\\s*\\{\\s*document\\s*\\}", l)
        if( length(i) ) l <- tail(l, -i)
        x <- paste0(l, collapse = "\n")
        message(sprintf("Processing citations in vignette '%s' ... ", basename(f)), appendLF = FALSE)
        res <- gsub_cite(x, roxybib)
        message(sprintf('OK [%s citations]', length(res)))
        res
      })
  NULL
}


# substitutes \cite commands with short or long citation
#' @importFrom digest digest
process_cite <- function(block, base_path, env, global_options){

  # to ensure 
  block <- block_backport(block)
  
  # get bibliography handler (cached)
  BIBS <- RoxyBibObject()

  tags_cite <- global_options$cite_tags %||% c('introduction', 'description', 'details', 'section', 'param')
  # backup original block value
  block0 <- block

  # 1. process all tags that can have \cite commands
  j_cite <- which(names(block) %in% tags_cite)
  if( length(j_cite) ){
    cite_res <- lapply(block[j_cite], gsub_cite, bibs = BIBS, short = TRUE, block = block)
    block[j_cite] <- lapply(cite_res, '[[', 'value')
    bibkeys <- unique(unlist(lapply(cite_res, '[[', 'bibkeys')))

    # 2. add parsed keys as references tags
    if( length(bibkeys) ){
      bibkeys <- unique(bibkeys)
      lapply(bibkeys, function(bk){
            block <<- append(block, list(references = sprintf('\\cite{%s}', bk)))
          })
    }
  }

  # 3. process references
  j_ref <- which(names(block) %in% 'references')
  if( length(j_ref) ){
    ref_res <- lapply(block[j_ref], gsub_cite, bibs = BIBS, short = FALSE, block = block)
    # process references as markdown
    block[j_ref] <- lapply(ref_res, function(x) tag_markdown(roxy_tag('references', val = x$value))$val)
  }

  block <- block_backport(block)
  # update in parsed block only if necessary
  if( digest(block) != digest(block0) ){
    # str(block)
    return(block)
  }
  block0
}

# find cite tags and resolve them against bibfiles
#' @importFrom stringr str_match_all str_trim
gsub_cite <- function(tag, bibs, short = TRUE, block = NULL){

  # cope for different types of tags
  field <- intersect(names(tag), c('value', 'val'))
  if( length(field) ){
    field <- field[1L]
    x <- tag[[field]]

  }else x <- tag

  # extract \cite tags
  cite_match <- str_match_all(x, "\\\\cite\\{([^}]+)\\}")
  # for each process citations
  res <- list(value = x, bibkeys = NULL)
  
  lapply(seq_along(cite_match), function(i){
        m <- cite_match[[i]]
        # no \cite command: return string untouched
        if( !length(m) ) return()
        
        # split into individual bibkeys
        keys <- strsplit(m[, 2L], '[;,]')
        # process each command
        mapply(function(cite_s, key){
              key <- str_trim(key)
              res$bibkeys <<- union(res$bibkeys, key)
              fkey <- bibs$format_cite(key, short = short, block = block)
              res$value[i] <<- gsub(cite_s, paste(fkey, collapse = if( short ) ', ' else "\n\n"), res$value[i], fixed = TRUE)
            }, m[, 1L], keys)
      })

  if( length(field) ){
    tag[[field]] <- res$value
    res$value <- tag
  }

  res
}

## Biobliography handler
RoxyBibObject <- local({
      .obj <- NULL
      function(base_path = NA, reset = FALSE){
        if( reset ) .obj <<- NULL
        # create or update instance
        if( is.null(.obj) ) .obj <<- RoxyBib$new(base_path)
        else .obj$set_path(base_path)
        .obj
      }
    })

#' @importFrom utils cite
RoxyBib <- R6::R6Class("RoxyBib", public = list(

      # data members
      base_path = NA,
      bibfiles = character(),
      bibs_loaded = character(),
      bibs = list(),
      bibentries = list(),

      # constructor
      initialize = function(path = NA) {
        self$set_path(path)
      },

      set_path = function(path = NA){
        if( is.na(path) ) return()
        self$base_path <- path
        ref_file <- file.path(self$base_path, 'inst/REFERENCES.bib')
        # append file to set of bibfiles if it exists
        if( file.exists(ref_file) ) self$add_bibfile(ref_file, prepend = TRUE)
      },

      add_bibfile = function(path, check = TRUE, block = NULL, prepend = FALSE){
        block <- block_backport(block)
        if( check && !file.exists(path) ) roxygen2:::block_warning(block, "could not find bibliograpy file ", path)
        npath <- normalizePath(path)
        self$bibfiles <- union(self$bibfiles, npath)
        if( prepend ) self$bibfiles <- union(npath, self$bibfiles)
        npath
      },

      load_bib = function(){
        path <- setdiff(self$bibfiles, self$bibs_loaded)[1L]
        if( is.na(path) || !file.exists(path) ) return(FALSE)
        library(bibtex)
        message(sprintf("Loading Bibtex file %s ... ", path), appendLF = FALSE)
        newbibs <- suppressMessages(suppressWarnings(read.bib2(file = path)))
        n <- length(self$bibs)
        self$bibs <- if( !length(self$bibs) ) newbibs else c(self$bibs, newbibs[setdiff(names(newbibs), names(self$bibs))])
        message(sprintf("OK [%i/%i new entries]", length(self$bibs) - n, length(newbibs)))
        self$bibs_loaded <- c(self$bibs_loaded, path)
        TRUE
      },

      # write package REFERENCES.bib file
      update_bibfile = function(file = NULL){
        if( !length(self$bibentries) ) return()
        file <- file %||% file.path(self$base_path, 'inst/REFERENCES.bib')
        message(sprintf("Writing file inst/%s", basename(file)))
        # create inst/ subdirectory if necessary
        dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
        write.bib(self$bibentries, file = file)
        file
      },

      # fetch bibitem from key
      get_bib = function(key, block = NULL){

        hit <- setNames(rep(NA_integer_, length(key)), key)
        while( anyNA(hit) ){
          bibkeys <- names(self$bibs)
          hit[key] <- match(key, bibkeys)
          if( anyNA(hit) && !self$load_bib() ) break
        }

        if( anyNA(hit) ){
          msg <- sprintf("Could not find bib entry for key(s) %s", paste(names(hit)[is.na(hit)], collapse = ', '))
          if( !is.null(block) ){
            block <- block_backport(block)
            roxygen2:::block_warning(block, msg)
            
          }else warning(msg)

        }

        self$bibs[names(hit)[!is.na(hit)]]
      },

      format_cite = function(key, short = TRUE, ...){
        # load bibitem
        res <- setNames(key, key)
        bibitems <- self$get_bib(key, ...)
        if( !length(bibitems) ) return(res)

        # add bibitems to set of used bibitems for final output in package REFERENCES.bib
        if( !length(self$bibentries) ) self$bibentries <- bibitems
        else self$bibentries <- c(self$bibentries, bibitems[setdiff(names(bibitems), names(self$bibentries))])

        # format accordingly
        if( !short ){
          # only either DOI or first URL
          lapply(names(bibitems), function(n){
                b <- bibitems[n]
                if( length(b$doi) ){
                  bibitems[n]$url <<- NULL
                  return()
                }
                urls <- b$urls
                if( !length(urls) || !nzchar(urls) ) return()
                bibitems[n]$url <<- strsplit(urls, " ", fixed = TRUE)[[1]][1L]
              })
          # format citations
          res[names(bibitems)] <- format(bibitems, style = 'text')
          res
        }else{
          # use utils::cite
          res[names(bibitems)] <- cite(names(bibitems), bibitems, textual = TRUE, longnamesfirst = FALSE)
          res
        }
      }

  ))


