
#' Get bcbio final output files
#'
#' Generates a tibble containing absolute paths to bcbio final files.
#'
#' @param d Path to `final` bcbio directory.
#' @return A tibble with the following columns:
#'   - fpath: path to file
#'   - ftype: file type. Can be one of:
#'     1. ensembl-batch
#'     2. mutect2-batch
#'     3. strelka2-batch
#'     4. vardict-batch
#'     5. ensemble-germ
#'     6. gatk-germ
#'     7. strelka2-germ
#'     8. vardict-germ
#'
#' @examples
#' \dontrun{
#' d <- "path/to/bcbio/final"
#' bcbio_outputs(d)
#' }
#' @export
bcbio_outputs <- function(d) {
  stopifnot(dir.exists(d))
  vcfs <- list.files(d, pattern = "\\.vcf.gz$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(vcfs) > 0)

  tibble::tibble(fpath = vcfs) %>%
    dplyr::mutate(bname = basename(.data$fpath)) %>%
    dplyr::select(.data$bname, .data$fpath) %>%
    dplyr::mutate(ftype = dplyr::case_when(
      grepl("germline-ensemble", .data$bname) ~ "ensemble-germ",
      grepl("ensemble", .data$bname) ~ "ensemble-batch",
      grepl("germline-vardict", .data$bname) ~ "vardict-germ",
      grepl("vardict-germline", .data$bname) ~ "OTHER",
      grepl("vardict", .data$bname) ~ "vardict-batch",
      grepl("germline-strelka2", .data$bname) ~ "strelka-germ",
      grepl("strelka2", .data$bname) ~ "strelka-batch",
      grepl("mutect2", .data$bname) ~ "mutect-batch",
      grepl("germline-gatk-haplotype", .data$bname) ~ "gatk-germ",
      grepl("manta", .data$bname) ~ "Manta",
      TRUE ~ "OTHER")) %>%
    dplyr::mutate(fpath = normalizePath(.data$fpath)) %>%
    dplyr::select(.data$ftype, .data$fpath)
}

#' Gather bcbio filepaths from two final directories into a single tibble
#'
#' Generates a tibble containing absolute paths to files from two bcbio final directories.
#'
#' @param d1 Path to first `final` bcbio directory.
#' @param d2 Path to second `final` bcbio directory.
#' @return A tibble with the following columns:
#'   - ftype: file type.
#'   - d1: final1 file path
#'   - d2: final2 file path
#'
#' @examples
#' \dontrun{
#' final1 <- "path/to/bcbio/final1"
#' final2 <- "path/to/bcbio/final2"
#' merge_bcbio_outputs(final1, final2)
#' }
#' @export
merge_bcbio_outputs <- function(d1, d2) {

  final1 <-
    bcbio_outputs(d1) %>%
    dplyr::filter(!.data$ftype %in% c("Manta", "OTHER"))
  final2 <-
    bcbio_outputs(d2) %>%
    dplyr::filter(!.data$ftype %in% c("Manta", "OTHER"))

  stopifnot(all(final1$ftype == final2$ftype))

  dplyr::left_join(final1, final2, by = "ftype") %>%
    utils::write.table(file = "", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}

#' Get final umccrise sample files
#'
#' Generates a tibble containing absolute paths to final umccrise sample files.
#'
#' @param d Path to `umccrised/<batch>__<tumor_name>` umccrise directory.
#' @return A tibble with the following columns:
#'   - fpath: path to file
#'   - ftype: file type. Can be one of:
#'     1. pcgr
#'     2. cpsr
#'
#' @examples
#' \dontrun{
#' um <- "path/to/umccrised/sample"
#' umccrise_outputs(um)
#' }
#' @export
umccrise_outputs <- function(d) {
  stopifnot(dir.exists(d))
  vcfs <- list.files(d, pattern = "\\.vcf.gz$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(vcfs) > 0)

  tibble::tibble(fpath = vcfs) %>%
    dplyr::mutate(bname = basename(.data$fpath)) %>%
    dplyr::select(.data$bname, .data$fpath) %>%
    dplyr::mutate(ftype = dplyr::case_when(
      grepl("-somatic.pcgr.pass.vcf.gz$", .data$bname) ~ "pcgr",
      grepl("-normal.cpsr.pass.vcf.gz", .data$bname) ~ "cpsr",
      TRUE ~ "OTHER")) %>%
    dplyr::mutate(fpath = normalizePath(.data$fpath)) %>%
    dplyr::select(.data$ftype, .data$fpath)
}

#' Gather umccrise filepaths from two different sample directories into a single tibble
#'
#' Generates a tibble containing absolute paths to files from two umccrise sample directories.
#'
#' @param d1 Path to first umccrise sample directory.
#' @param d2 Path to second umccrise sample directory.
#' @return A tibble with the following columns:
#'   - ftype: file type.
#'   - d1: sample1 file path
#'   - d2: sample2 file path
#'
#' @examples
#' \dontrun{
#' um1 <- "path/to/umccrised/sample1"
#' um2 <- "path/to/umccrised/sample2"
#' merge_umccrise_outputs(um1, um2)
#' }
#' @export
merge_umccrise_outputs <- function(d1, d2) {

  um1 <-
    umccrise_outputs(d1) %>%
    dplyr::filter(!.data$ftype %in% c("OTHER"))
  um2 <-
    umccrise_outputs(d2) %>%
    dplyr::filter(!.data$ftype %in% c("OTHER"))

  stopifnot(all(um1$ftype == um2$ftype))

  dplyr::left_join(um1, um2, by = "ftype") %>%
    utils::write.table(file = "", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}
