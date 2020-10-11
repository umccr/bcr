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
