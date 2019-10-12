
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
#' @param sample Sample name.
#' @return A tibble with the following columns:
#'   - sample: sample name
#'   - ftype: file type
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
merge_bcbio_outputs <- function(d1, d2, sample) {

  final1 <-
    bcbio_outputs(d1) %>%
    dplyr::filter(!.data$ftype %in% c("Manta", "OTHER"))
  final2 <-
    bcbio_outputs(d2) %>%
    dplyr::filter(!.data$ftype %in% c("Manta", "OTHER"))

  stopifnot(all(final1$ftype == final2$ftype))

  dplyr::left_join(final1, final2, by = "ftype") %>%
    dplyr::mutate(sample = sample) %>%
    dplyr::select(.data$sample, .data$ftype, .data$fpath.x, .data$fpath.y) %>%
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
#' @param sample Sample name.
#' @return A tibble with the following columns:
#'   - sample: sample name.
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
merge_umccrise_outputs <- function(d1, d2, sample) {

  um1 <-
    umccrise_outputs(d1) %>%
    dplyr::filter(!.data$ftype %in% c("OTHER"))
  um2 <-
    umccrise_outputs(d2) %>%
    dplyr::filter(!.data$ftype %in% c("OTHER"))

  stopifnot(all(um1$ftype == um2$ftype))

  dplyr::left_join(um1, um2, by = "ftype") %>%
    dplyr::mutate(sample = sample) %>%
    dplyr::select(.data$sample, .data$ftype, .data$fpath.x, .data$fpath.y) %>%
    utils::write.table(file = "", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}


#' Read YAML configs from two bcbio runs
#'
#' Generates a list containing absolute paths to variant files for
#' two bcbio runs.
#'
#'
#' @param conf1 Path to `final/config/<timestamp>_project.yaml` for run1.
#' @param conf2 Path to `final/config/<timestamp>_project.yaml` for run2.
#' @return A list with the following elements:
#'   - 'batch_name', 'tumor_name', 'normal_name': batch/tumor/normal names.
#'   - 'variant_files': file type. Can be one of:
#'     1. ensemble_som
#'     2. mutect2_som
#'     3. strelka2_som
#'     4. vardict_som
#'     5. ensemble_ger
#'     6. gatk-haplotype_ger
#'     7. strelka2_ger
#'     8. vardict_ger
#'     9. manta_som-sv
#'
#' @examples
#' \dontrun{
#' conf1 <- "path/to/bcbio_run1/final/config/<timestamp>_project.yaml"
#' conf2 <- "path/to/bcbio_run2/final/config/<timestamp>_project.yaml"
#' read_bcbio_configs(conf1, conf2)
#' }
#' @export
read_bcbio_configs <- function(conf1, conf2) {
  b1 <- read_bcbio_config(conf1)
  b2 <- read_bcbio_config(conf2)

  ### variant callers
  vc1 <- b1$varcallers
  vc2 <- b2$varcallers
  stopifnot(all(vc1$caller_name2 == vc2$caller_name2))

  caller_nms <- unique(vc1$caller_name2)
  caller_list <- purrr::map(caller_nms, function(caller) {
    list(run1 = vc1$fpath[vc1$caller_name2 == caller],
         run2 = vc2$fpath[vc2$caller_name2 == caller])}) %>%
    purrr::set_names(caller_nms)

  ### check stuff: aligner + genome_build can differ
  stopifnot(b1$batch_name == b2$batch_name,
            b1$tumor_name == b2$tumor_name,
            b1$normal_name == b2$normal_name,
            b1$analysis_type == b2$analysis_type)

  out <- list(
    batch_name = b1$batch_name,
    tumor_name = b1$tumor_name,
    normal_name = b1$normal_name,
    aligner = list(run1 = b1$aligner,
                   run2 = b2$aligner),
    genome_build = list(run1 = b1$genome_build,
                        run2 = b2$genome_build),
    analysis_type = b1$analysis_type,
    variant_files = caller_list)

  out

}
