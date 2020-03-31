
#' @export
namespace2_roclet <- function() {
  roc <- roclet("namespace2")
  # Add roclet_rd class to inherit its methods
  class(roc) <- append(class(roc), 'roclet_namespace', after = 1L)
  roc

}

#' Alternative NAMESPACE Roclet
#'
#' Same as [roxygen2::roclet_process.roclet_namespace], but allow for keeping the
#' NAMESPACE entries unsorted.
#' This is useful to avoid conflicts in generics.
#'
#' @inheritParams roxygen2::roclet_process
#' @export
#' @rdname namespace_unsorted_roclet
roclet_process.roclet_namespace2 <- function(x, blocks, env, base_path = env){
  opts <- load_options(base_path)
  if( !isTRUE(opts[["namespace_unsorted"]]) ) return(roxygen2::roclet_process.roclet_namespace(x, blocks, env, base_path))
  blocks_to_ns2(blocks, env)

}

#' @importFrom purrr map
blocks_to_ns2 <- function(blocks, env, import_only = FALSE) {
  block_to_ns <- get("block_to_ns", envir = asNamespace("roxygen2"))
  block_to_ns <- get("block_to_ns", envir = asNamespace("roxygen2"))
  lines <- map(blocks, block_to_ns, env = env, import_only = import_only)
  lines <- unlist(lines) %||% character()

  unique(lines)

}
