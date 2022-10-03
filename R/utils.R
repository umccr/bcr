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

#' Guess Type of File
#'
#' Guesses type of file based on suffix.
#'
#' @param x File path.
#' @return A character string: FASTQ, BAM, VCF or OTHER.
#' @export
guess_file_type <- function(x) {
  dplyr::case_when(
    grepl("\\.bam$", x, ignore.case = TRUE) ~ "BAM",
    grepl("\\.bai$", x, ignore.case = TRUE) ~ "BAMindex",
    grepl("\\.fastq.gz$", x, ignore.case = TRUE) ~ "FASTQ",
    grepl("\\.fastq$", x, ignore.case = TRUE) ~ "FASTQ",
    grepl("\\.fq$", x, ignore.case = TRUE) ~ "FASTQ",
    grepl("\\.fq\\.gz$", x, ignore.case = TRUE) ~ "FASTQ",
    grepl("manifest\\.txt$", x, ignore.case = TRUE) ~ "Manifest",
    grepl("\\.md5$", x, ignore.case = TRUE) ~ "MD5",
    grepl("md5\\.txt$", x, ignore.case = TRUE) ~ "MD5txt",
    grepl("\\.vcf$", x, ignore.case = TRUE) ~ "VCF_unz",
    grepl("\\.vcf\\.gz$", x, ignore.case = TRUE) ~ "VCF_gz",
    grepl("\\.tbi$", x, ignore.case = TRUE) ~ "VCFindex",
    TRUE ~ "OTHER")
}
