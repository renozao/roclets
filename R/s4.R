
block_get_object_class <- function(block){
  class(block$object)
  
}
block_object_is <- function(block, class){
  any(block_get_object_class(block) %in% class)
  
}

#' @export
roxy_tag_parse.roxy_tag_inline <- function(x) {
  tag_toggle(x)

}

# taken from roxygen2::escape
escape <- function (x){
    x1 <- gsub("\\", "\\\\", x, fixed = TRUE, useBytes = TRUE)
    x2 <- gsub("%", "\\%", x1, fixed = TRUE, useBytes = TRUE)
    x2
}

`.roxy_tag<-` <- function(block, name, value){
  hit <- which(roxygen2:::block_tags(block) %in% name)
  if( !length(hit) ){
    if( !is.null(value) ) block$tags <- c(block$tags, list(value))
  }else{
    stopifnot(length(hit) == 1L)
    block$tags[[hit]] <- value
  }
  block
  
}

#' Builds a data.frame Describing the roxygen blocks that can be amended with mini sections
#' extracted from other blocks, so as to create cross-references between man pages of 
#' generics, methods and classes.
#' 
#' @noRd
.build_target_block <- function(blocks){
  target_blocks <- mapply(seq_along(blocks), blocks, FUN = function(i, block){
    cl <- intersect(block_get_object_class(block), 
                    c("s4class", "s4generic", "s3generic"))
    obj <- block_object(block)
    is_valid_target <- block_has_tags(block, c("title", "rdname", "describeIn"))
    is_exported <- block_has_tags(block, "export")
    is_inline <- block_has_tags(block, "inline")
    if( !length(cl) ) return()
    if( cl %in% c("s4generic", "s4method") ) name <- as.character(obj$value@generic)
    else if( cl %in% c("s3method") ) name <- as.character(attr(obj$value, "s3method")[1])
    else {
      name <- obj[["topic"]]
      
    }
    rdname <- block_get_tag_value(block, "rdname") %||% name
    data.frame(i = i, target = name, type = cl, rdname = rdname, 
               is_exported = is_exported, is_valid = is_valid_target, is_inline = is_inline,
               stringsAsFactors = FALSE)
    
  }, SIMPLIFY = FALSE)
  target_blocks <- target_blocks[lengths(target_blocks) > 0]
  res <- do.call(rbind, target_blocks)
  valid_exported_targets <- unique(res[res[, "is_valid"], "target"])
  res <- res[res[, "is_valid"] | res[, "target"] %in% valid_exported_targets, , drop = FALSE]
  res
  
}

#' Finds the set of target roxygen blocks for a given object.
#' 
#' Returns the subset of blocks where mini-descriptions should be included
#' The first block is the primary target block in which the source block
#' gets merged (via rdname) if it does not require an Rd file on its own.
#' 
#' Cases:
#'  * S3 methods:
#'    - targets: generic, dispatching class
#'  * S4 methods:
#'    - 
#'   
#' @noRd 
.find_target_blocks <- function(block, target_blocks){
  block_obj <- block_object(block)
  classes_to_process <- c("s4method", "s3method")
  obj_class <- block_get_object_class(block)
  if( !obj_class %in% classes_to_process ) return()
  # if( !length(obj_class) || block_has_tags(block, c("describeIn", "rdname")) ) return()
  
  local_s4classes <- unique(target_blocks[target_blocks[, "type"] %in% "s4class", "target"])
  local_s4generic <- target_blocks[target_blocks[, "type"] %in% "s4generic", , drop = FALSE]
  if( obj_class == "s4method" ){
    obj_generic <- as.character(block_obj$value@generic)
    s4class_targets <- intersect(paste0(block_obj$value@defined, "-class"), local_s4classes)
    s4generic_targets <- local_s4generic[local_s4generic[, "target"] %in% obj_generic, , drop = FALSE]
    # if( s4gen %in% local_s4generic ) target_name <- c(s4gen, target_name)
    # else target_name <- c(target_name, s4gen)
    target_name <- c(s4generic_targets[s4generic_targets[, "is_inline"], "target"], 
                     s4class_targets,
                     s4generic_targets[!s4generic_targets[, "is_inline"], "target"])
    target_type <- c("s4class", "s4generic")
    
  }else{
    target_name <- as.character(attr(block_obj$value, "s3method")[2:1])
    s4class_targets <- intersect(paste0(target_name[1], "-class"), local_s4classes)
    target_name <- c(s4class_targets, target_name)
    target_type <- c("s3generic", "s4class", "s4generic")
    
  }
  # look for the targets where to put add a minidesc
  target_blocks <- target_blocks[target_blocks[, "target"] %in% target_name & 
                                   target_blocks[, "type"] %in% target_type, , drop = FALSE]
  target_blocks <- target_blocks[order(match(target_blocks[, "target"], target_name)), , drop = FALSE]
  
  # remove the target block that might already be declared in the block
  original_rdname <- block_get_tag_value(block, "describeIn") %||% block_get_tag_value(block, "rdname")
  target_blocks <- target_blocks[!target_blocks[, "rdname"] %in% original_rdname, , drop = FALSE]
  
  target_blocks
  
}

.find_block <- function(blocks, topic = NULL, class = NULL, tag = NULL){
  hit <- integer()
  if( !is.null(topic) ) hit <- c(hit, which(sapply(blocks, function(block) grepl(topic, block$object$topic %||% ""))))
  if( !is.null(class) ){
    hit <- c(hit, which(sapply(blocks, function(block) class %in% class(block$object))))
    
  }
  if( !is.null(tag) ){
    hit <- c(hit, which(sapply(blocks, function(block) block_has_tags(block, tag))))
    
  }
  if( !length(hit) ) return(list())
  
  list(blocks = blocks[hit], index = hit)
  
}

# add aliases for exported methods of internal classes
# documentation of exported S4 methods
.add_internal_blocks <- function(blocks, env, base_path = env){
  # find all S4 classes: create internal Rd file for them if needed
  kw_blocks <- .find_block(blocks, tag = "keywords")
  edited <- mapply(kw_blocks[["blocks"]], kw_blocks[["index"]], FUN = function(block, index){
    to_internal <- "internal" %in% block_get_tag_value(block, tag = "keywords") && 
      length(setdiff(roxygen2:::block_tags(block), c("usage", ".formals", "backref"))) == 1L
    if( !to_internal ) return(FALSE)
    block <- blocks[[index]]
    .roxy_tag(block, "rdname") <- roxy_tag_parse(roxy_tag("rdname", "Internal-objects"))
    blocks[[index]] <<- block
    TRUE
  })
  if( any(edited) ){
    internal_chunk <- "#' Internal Undocumented Entries
#' This man page is automatically generated to gather all internal functions, classes and objects that are 
#' not meant to be called outside of the package.
#' @name Internal-objects
#' @keywords internal
NULL
"
    internal_dump <- parse_text(internal_chunk)
    blocks <- c(blocks, internal_dump)
    
  }
  blocks
  
  # pkg_classes <- getClasses(env, inherits = FALSE)
  # doc_classes <- sapply(.find_block(blocks, class = "s4class"), function(x) x$object$topic)
  # internal_classes <- setdiff(pkg_classes, sub("-class$", "", doc_classes))
  # internal_block <- roxy_block(tags = roxy_tag_parse("#' @rdname Internal-objects\n#' @keywords internal"))
  # c(list(internal_block), lapply(internal_classes, function(class){
  #   mtext <- showMethods(class = class, where = env, inherited = FALSE, printTo = FALSE)
  #   gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
  #                                     "\\2", mtext[grep("^Function", mtext)] )
  # 
  # }))

}

# @inline - describe methods in related object files
# When the roxygen data associated with them is small enough, then we 
# avoid creating a dedicated file for them.
.add_method_mini_sections <- function(blocks, env, base_path){
  
  opts <- load_options(base_path)
  disabled <- identical(opts[["compact"]], FALSE)
  if( disabled ) return(blocks)
  
  # build inline set
  # inline_set <- (lapply(blocks, function(x){
  #   x <- block_backport(x)
  #   if( block_has_tags(x, "inline") && class(x$object) == 's4generic' ){
  #     setNames(as.character(x$object$value@generic), block_get_tag_value(x, "rdname"))
  #   }
  # }))
  # inline_set <- do.call(c, inline_set)

  # gather all the potential targets: S4 classes and S3/S4 generics
  target_blocks <- .build_target_block(blocks)
  if( !nrow(target_blocks) ) return(blocks)
  # str(target_blocks)
  
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    hash <- digest(block)
    block <- block_backport(block)
    dest_blocks <- .find_target_blocks(block, target_blocks)
    if( is.null(dest_blocks) || !nrow(dest_blocks) ) next;
    
    primary_target_rdname <- dest_blocks[1L, "rdname"]
    # determine if the method deserves a separate documentation:
    # if not, then replace the block by its mini-description version, forcing it to be merged
    # into its first target block.
    block_obj <- block_object(block)
    obj_class <- block_get_object_class(block)
    # if( obj_class %in% "s4method" && as.character(block_obj$value@generic) %in% "run" ) browser()
    need_separate_doc <- block_has_tags(block, c("section", "describeIn", "rdname")) && !block_has_tags(block, c("inline"))
    block_with_minidesc <- build_compact_block(block, base_path = base_path, full = !need_separate_doc)
    if( !need_separate_doc ){
      .roxy_tag(block_with_minidesc, "rdname") <- roxy_tag_parse(roxy_tag("rdname", primary_target_rdname))
      dest_blocks <- tail(dest_blocks, -1L)
      block <- block_with_minidesc
      
    }
    # add all the blocks tags to the remaining target tags
    for( i_block in dest_blocks[, "i"] ){
      # b_tags <- block_with_minidesc$tags
      if( block_has_tags(block_with_minidesc, "method_minidesc") ){
        blocks[[i_block]]$tags <- c(blocks[[i_block]]$tags, list(block_get_tag(block_with_minidesc, "method_minidesc")))
      }
      # .roxy_tag(block_with_minidesc, "aliases") <- NULL

    }

    if (length(block) == 0)
      next

    if( digest(block) != hash ) blocks[[i]] <- block
  }
  
  blocks
}

#' @importFrom checkmate assert_subset
build_compact_block <- function(block, base_path, full = TRUE){
  
  # extract title, description and details from the @describeIn tag if present
  describeIn_description <- block_get_tag_value(block, "describeIn")[["description"]]
  if( !is.null(describeIn_description) ){
    block_desc <- roxygen2:::parse_tags(roxygen2:::parse_description(list(roxy_tag("", describeIn_description))))
    assert_subset(roxygen2:::block_tags(block_desc), c("title", "description", "details"))
    block$tags <- roxygen2:::compact(c(block$tags, block_desc))
    .roxy_tag(block, "describeIn") <- NULL
    
  }
  
  # build from title, description and details
  title <- block_get_tag_value(block, "title")
  description <- block_get_tag_value(block, "description")
  details <- block_get_tag_value(block, "details")
  
  val <- title
  if( !is.null(description) && !identical(description, title) ) val <- paste0(val, "\n\n", description)
  if( full && !is.null(details) ) val <- paste0(val, "\n\n", details)
  # buid mini-description if possible
  if( !is.null(val) ){
    info <- build_label2(block_object(block))
    tag <- roxy_tag("method_minidesc", raw = "", val = list(type = info$type, label = info$label, desc = val))
    .roxy_tag(block, "title") <- NULL
    .roxy_tag(block, "description") <- NULL
    .roxy_tag(block, "details") <- NULL
    .roxy_tag(block, "method_minidesc") <- tag
    
  }
  
  block
  
}

roxy_tag_rd.roxy_tag_method_minidesc <- function(x, base_path, env) {
  value <- x$val
  if( !length(value$desc) ) roxy_tag_warning(x, "Empty method description")
  label <- value$label
  stopifnot( !is.null(names(label)) )
  
  rd_section_method_minidesc(type = value$type, entry = names(label), label = label, desc = value$desc)
  
}

#' @importFrom checkmate assert_string assert_named
rd_section_method_minidesc <- function(type, entry, label, desc) {
  assert_string(type)
  assert_string(desc)
  assert_string(entry)
  assert_string(label)
  assert_named(label)

  entry <- list(type = type, entry = entry, label = label, desc = desc)
  rd_section("method_minidesc", setNames(list(entry), type))
  
}

#' @export
merge.rd_section_method_minidesc <- function(x, y, ...) {
  
  .merge <- function(x, y){
    if( is.null(x) ) return(y)
    if( is.null(y) ) return(x)
    list(
      type = x$type,
      entry = c(x$entry, y$entry),
      label = c(x$label, y$label),
      desc = c(x$desc, y$desc)
    )
  }
  
  types <- union(names(x$value), names(y$value))
  val <- sapply(types, function(t){
    .merge(x$value[[t]], y$value[[t]])
  }, simplify = FALSE)

  rd_section("method_minidesc", val)
  
}

#' @export
format.rd_section_method_minidesc <- function(x, ...) {
  
  .format <- function(type, x, ...){
      title <- switch(type,
      generic = "Generics",
      method = "Methods (by generic)",
      "function" = "Functions",
      stop("Unsupported type ", type)
    )
  
    if( type %in% "method" ){
      generics <- x$entry
      stopifnot( !is.null(generics) )
      generics <- split(seq_along(x$label), generics)
      if( length(generics) > 1L && any(lengths(generics) > 1L) ){ 
        res <- mapply(names(generics), generics, FUN = function(g, i){
          paste0(
            "\\bold{", g, ":}\n\n",
            "\\itemize{\n",
            paste0("\\item \\code{", escape(x$label[i]), "}: ", x$desc[i],
                   collapse = "\n\n"),
            "\n}\n"
          )
        })
        res <- paste0(
            "\\section{", title, "}{\n",
            paste0(res, collapse = "\n\n"),
            "\n}\n")
        return(res)
        
      }
    }
    paste0(
      "\\section{", title, "}{\n",
      "\\itemize{\n",
      paste0("\\item \\code{", escape(x$label), "}: ", x$desc,
        collapse = "\n\n"),
      "\n}}\n"
    )
    
  }
  
  # str(x$value)
  fmt <- mapply(names(x$value), x$value, FUN = .format, ...)
  fmt <- fmt[order(match(names(fmt), c("class", "generic", "method", "function")))]
  paste0(fmt, collapse = "\n\n")
  
}

build_label2 <- function(src) {
  src_type <- class(src)[1]

  if (src_type == "s4generic") {
    # Label S4 methods in class with their generic
    type <- "generic"
    label <- as.character(src$value@generic)
  } else if (src_type == "s4method") {
    # Label S4 methods in generic with their signature
    type <- "method"
    sig <- src$value@defined
    # if (length(sig) == 1) {
    #   label <- as.character(sig)
    # } else {
      generic <- as.character(src$value@generic)
      label <- setNames(sprintf("%s(%s)", generic, paste0(names(sig), " = ", sig, collapse = ",")), generic)
    # }
  } else if (src_type == "s3method") {
    # Assuming you document S3 methods in the class constructor
    type <- "method"
    generic <- attr(src$value, "s3method")[1]
    label <- setNames(sprintf("%s(%s)", generic, attr(src$value, "s3method")[2]), generic)
  } else {
    # Otherwise just fallback to function + topic
    type <- "function"
    label <- src$topic
  }

  list(type = type, label = label)
  
}
