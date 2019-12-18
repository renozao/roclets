# Bibliography support
#
# Author: Renaud Gaujoux
###############################################################################

## Handling of bibliography-related tags
#' Extra Roxygen Tags
#'
#' @param x roxy_tag object
#'
#' @name rd2_tag
NULL

#' @describeIn rd2_tag Provides support for tags `@@cite` that should contain Bibtex 
#' entry keys, which are looked-up in the `inst/REFERENCES.bib` file or external
#' Bibtex files specified via tag @@bibliography
#' @export
roxy_tag_parse.roxy_tag_cite <- function(x) {
  # convert into @references \cite{<entry1>} \cite{<entry2>} ...
  x <- tag_words(x)
  roxy_tag('references', raw = x$raw, val = sprintf("\\cite{%s}", x$val))
  
}

#' @describeIn rd2_tag Provides support for tag `@@bibliography`
#' @importFrom pkgload pkg_path
#' @export
roxy_tag_parse.roxy_tag_bibliography <- function(x) {
  # add path to bib file to current BibObject handler
  pkg_dir <- tryCatch(pkg_path(x[["file"]]), 
                      error = function(e){
                        NA_character_
                      })
  bib <- RoxyBibObject(base_path = pkg_dir)
  x <- tag_words(x)
  x <- block_backport(x)
  bib$add_bibfile(x$val, block = x)
  x
  
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
        message(sprintf("* Processing citations in vignette '%s' ... ", basename(f)), appendLF = FALSE)
        res <- gsub_cite(x, roxybib)
        message(sprintf('OK [%s citations: %s]', length(res[["bibkeys"]]), str_excerpt(res[["bibkeys"]])))
        res
      })
  NULL
}


.get_block_tags <- function(block){
  sapply(block[["tags"]], "[[", "tag")
  
}

# substitutes \cite commands with short or long citation
#' @importFrom digest digest
process_cite <- function(block, base_path, env){

  # to ensure 
  block <- block_backport(block)
  
  # get bibliography handler (cached)
  BIBS <- RoxyBibObject()

  tags_cite <- c('introduction', 'description', 'details', 'section', 'param')
  # backup original block value
  block0 <- block

  # 1. process all tags that can have \cite commands
  j_cite <- which(.get_block_tags(block) %in% tags_cite)
  if( length(j_cite) ){
    cite_res <- lapply(block[["tags"]][j_cite], gsub_cite, bibs = BIBS, short = TRUE, block = block)
    block[["tags"]][j_cite] <- lapply(cite_res, '[[', 'value')
    bibkeys <- unique(unlist(lapply(cite_res, '[[', 'bibkeys')))

    # 2. add parsed keys as references tags
    if( length(bibkeys) ){
      bibkeys <- unique(bibkeys)
      lapply(bibkeys, function(bk){
        cite_statement <- sprintf('\\cite{%s}', bk)
        block[["tags"]] <<- append(block[["tags"]], list(roxy_tag("references", raw = cite_statement, val = cite_statement)))
      })
    }
  }

  # 3. process references
  j_ref <- which(.get_block_tags(block) %in% 'references')
  if( length(j_ref) ){
    ref_res <- lapply(block[["tags"]][j_ref], gsub_cite, bibs = BIBS, short = FALSE, block = block)
    # process references as markdown
    block[["tags"]][j_ref] <- lapply(ref_res, "[[", "value") #tag_markdown(roxy_tag('references', raw = x$value$val, val = x$value$val))$val)
    
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
  cite_match <- str_match_all(x, "(\\\\cite(p)?\\{([^}]+)\\}|\\[@([^]]+)\\])")
  # for each process citations
  res <- list(value = x, bibkeys = NULL)
  
  lapply(seq_along(cite_match), function(i){
        m <- cite_match[[i]]
        # no \cite command: return string untouched
        if( !length(m) ) return()
        
        # split into individual bibkeys
        matched_keys <- ifelse(is.na(m[, 4L]), m[, 5L], m[, 4L])
        keys <- strsplit(matched_keys, '[;,@]')
        #print(keys)
        # process each command
        mapply(function(cite_s, key, with_p){
              key <- str_trim(key)
              res$bibkeys <<- union(res$bibkeys, key)
              fkey <- bibs$format_cite(key, short = short, block = block, with_brackets = with_p)
              res$value[i] <<- gsub(cite_s, paste(fkey, collapse = if( short ) ', ' else "\n\n"), res$value[i], fixed = TRUE)
            }, m[, 1L], keys, !is.na(m[, 3L]))
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
        else if(!is.na(base_path)) .obj$set_path(base_path)
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
        if( !length(setdiff(self$base_path, path)) ) return()
        self$base_path <- path
        ref_file <- file.path(self$base_path, 'inst/REFERENCES.bib')
        # append file to set of bibfiles if it exists
        if( file.exists(ref_file) ) self$add_bibfile(ref_file, prepend = TRUE)
      },

      add_bibfile = function(path, check = TRUE, block = NULL, prepend = FALSE){
        block <- block_backport(block)
        if( !file.exists(path) ){
          if( check ) roxy_tag_warning(block, "could not find bibliograpy file ", path)
          return()
          
        }
        npath <- normalizePath(path)
        message("* Registering bibliography file: ", npath, appendLF = FALSE)
        self$bibfiles <- union(self$bibfiles, npath)
        if( prepend ) self$bibfiles <- union(npath, self$bibfiles)
        message(sprintf(" [%s]", match(npath, self$bibfiles)))
        npath
      },

      load_bib = function(){
        path <- setdiff(self$bibfiles, self$bibs_loaded)[1L]
        if( !length(path) || is.na(path) || !file.exists(path) ) return(FALSE)
        library(bibtex)
        i_bib <- match(path, self$bibfiles)
        message(sprintf("(BIB[%s]: ", i_bib), appendLF = FALSE)
        newbibs <- suppressMessages(suppressWarnings(read.bib2(file = path)))
        n <- length(self$bibs)
        self$bibs <- if( !length(self$bibs) ) newbibs else c(self$bibs, newbibs[setdiff(names(newbibs), names(self$bibs))])
        message(sprintf("%i/%i new entries) ", length(self$bibs) - n, length(newbibs)), appendLF = FALSE)
        self$bibs_loaded <- c(self$bibs_loaded, path)
        TRUE
      },

      # write package REFERENCES.bib file
      update_bibfile = function(file = NULL){
        if( !length(self$bibentries) ) return()
        file <- file %||% file.path(self$base_path, 'inst/REFERENCES.bib')
        message(sprintf("* Writing file inst/%s", basename(file)))
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
            roxy_tag_warning(block, msg)
            
          }else warning(msg)

        }

        self$bibs[names(hit)[!is.na(hit)]]
      },

      format_cite = function(key, short = TRUE, with_brackets = FALSE, ...){
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
          
        }else{
          # use utils::cite
          res[names(bibitems)] <- cite(names(bibitems), bibitems, textual = TRUE, longnamesfirst = FALSE)
          
        }
        
        if( with_brackets ) res <- paste0("(", res, ")")
        res
        
      }

  ))


