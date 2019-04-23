#' List AWS S3 Objects
#'
#' Lists objects inside S3 bucket.
#'
#' @param b S3 path.
#' @param recursive Recursively list objects within given path.
#' @return A tibble with file basename, complete S3 file path, and file type.
#' @examples
#' \dontrun{
#' b <- "s3://my-crazy-bucket/dir1"
#' list_s3_objects(b)
#' }
#' @export
list_s3_objects <- function(b, recursive = FALSE) {
  stopifnot(Sys.which("aws") != "")
  stopifnot(is.logical(recursive))
  rec <- ifelse(recursive, "--recursive", "")

  s3_query <- system(command = paste("aws s3 ls", b, rec), intern = TRUE)

  tibble::tibble(all_cols = s3_query) %>%
    dplyr::filter(!grepl("PRE .*", .data$all_cols)) %>% # discard directories - use recursive if needed
    tidyr::separate(col = .data$all_cols,
                    into = c("date", "time", "size", "fname"),
                    sep = " +", convert = TRUE) %>%
    dplyr::mutate(fname_full = file.path(b, .data$fname),
                  fname_type = guess_file_type(.data$fname)) %>%
    dplyr::select(basename = .data$fname, .data$fname_full, .data$fname_type)

}
