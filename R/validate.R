#' Guess Type of File
#'
#' Guesses type of file.
#'
#' @param file File path.
#' @return A character string: FASTQ, BAM, VCF or OTHER.
#' @export
guess_file_type <- function(file) {
  dplyr::case_when(
    grepl("fastq.gz$", file, ignore.case = TRUE) ~ "FASTQ",
    grepl("fastq$", file, ignore.case = TRUE) ~ "FASTQ",
    grepl("fq$", file, ignore.case = TRUE) ~ "FASTQ",
    grepl("fq.gz$", file, ignore.case = TRUE) ~ "FASTQ",
    grepl("bam$", file, ignore.case = TRUE) ~ "BAM",
    grepl("vcf$", file, ignore.case = TRUE) ~ "VCF",
    grepl("vcf.gz$", file, ignore.case = TRUE) ~ "VCF",
    TRUE ~ "OTHER")
}
