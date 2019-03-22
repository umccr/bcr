#' List Files in knitr kable
#'
#' Lists the files in the given directory in a single-column
#' `knitr::kable`.
#'
#' @param ... Passed to `list.files`
#' @param row.names Passed to `knitr::kable`
#' @return A `knitr::kable`
#' @examples
#' \dontrun{
#' lf("/path/to/foo")
#' lf("/path/to/foo", row.names = FALSE)
#' }
#' @export
lf <- function(..., row.names = TRUE) {
  data.frame(fname = list.files(...)) %>% knitr::kable(row.names = row.names)
}

#' Call system tree
#'
#' Calls the system tree.
#'
#' @param x File path.
#' @param ... Passed to `paste0`. Stuff to put after `tree_`.
#' @return A `tree` representation of `x`.
#' @export
tree <- function(x, ...) {
  cat(
    system(paste0("tree ", ..., x), intern = TRUE),
    sep = "\n")
}
