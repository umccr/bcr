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

#' Read fqtools output
#'
#' Reads output from fqtools.
#'
#' @param f Path to file output by fqtools.
#'
#' @return Tibble with two columns: validation verdict and input file name.
#' @export
read_fqtools <- function(f) {
  x <- readr::read_lines(f, n_max = 1)
  tibble::tibble(verdict = x, fname = sub("\\.fqtools_validate\\.txt", "", basename(f))) %>%
    dplyr::mutate(verdict = ifelse(x == "OK", "OK", "ERROR"))
}

#' Read samtools quickcheck output
#'
#' Reads output from samtools quickcheck.
#'
#' @param f Path to file output by samtools quickcheck.
#'
#' @return Tibble with two columns: validation verdict and input file name.
#' @export
read_qcheck <- function(f) {
  x <- readr::read_lines(f, n_max = 1)
  tibble::tibble(verdict = x, fname = sub("\\.quickcheck\\.txt", "", basename(f))) %>%
    dplyr::mutate(verdict = ifelse(x == "ok", "OK", "ERROR"))
}

#' Read bcftools query output
#'
#' Reads output from bcftools query for sample list.
#'
#' @param f Path to file output by bcftools query.
#'
#' @return Tibble with three columns: validation verdict, input file name, and basename.
#' @export
read_bcftools_querysn <- function(f) {
  x <- readr::read_lines(f)
  if (x[1] != "ok") {
    stop(paste(f, "failed! Please check!"))
  }
  tibble::tibble(fname = sub("\\.query_sn\\.txt", "", basename(f))) %>%
    dplyr::mutate(verdict = "OK") %>%
    dplyr::select(.data$verdict, .data$fname)
}

#' Read md5sum output
#'
#' Reads output md5sum.
#'
#' @param f Path to md5sum file.
#'
#' @return Tibble with two columns: md5sum and basename of input file.
#' @export
read_md5sum <- function(f) {
  readr::read_table2(f, col_names = c("md5sum", "fname"), col_types = "cc") %>%
    dplyr::mutate(fname = basename(.data$fname))
}
