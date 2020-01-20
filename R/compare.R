
#' Get bcbio final output files
#'
#' Generates a tibble containing absolute paths to bcbio final files.
#'
#' @param d Path to `<bcbio/final>` directory.
#' @return A tibble with the following columns:
#'   - vartype: variant type. Can be one of:
#'     - SNV (single-nucleotide/indel variants)
#'     - SV (structural variants)
#'   - flabel: file label (e.g. ensemble, manta, vardict etc.)
#'   - fpath: path to file
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
    dplyr::mutate(
      bname = sub("\\.vcf.gz$", "", basename(.data$fpath)),
      fpath = normalizePath(.data$fpath),
      flabel = dplyr::case_when(
        grepl("germline-ensemble", .data$bname) ~ "ens-germ_bc",
        grepl("ensemble", .data$bname) ~ "ens-batch_bc",
        grepl("germline-vardict", .data$bname) ~ "vardict-germ_bc",
        grepl("vardict-germline", .data$bname) ~ "vardict-germ2_bc",
        grepl("vardict", .data$bname) ~ "vardict-batch_bc",
        grepl("germline-strelka2", .data$bname) ~ "strelka2-germ_bc",
        grepl("strelka2", .data$bname) ~ "strelka2-batch_bc",
        grepl("mutect2", .data$bname) ~ "mutect2-batch_bc",
        grepl("germline-gatk-haplotype", .data$bname) ~ "gatk-germ_bc",
        grepl("manta", .data$bname) ~ "manta_bc",
        TRUE ~ "IGNORE_ME"),
      vartype = dplyr::case_when(
        flabel == "IGNORE_ME" ~ "IGNORE_ME",
        flabel == "manta_bc" ~ "SV",
        TRUE ~ "SNV")) %>%
    dplyr::select(.data$vartype, .data$flabel, .data$fpath)
}

#' Gather bcbio filepaths from two bcbio final directories into a single tibble
#'
#' Generates a tibble containing absolute paths to (common) files from two bcbio final directories.
#'
#' @param d1 Path to first `<bcbio/final>` directory.
#' @param d2 Path to second `<bcbio/final>` directory.
#' @param sample Sample name.
#' @return A tibble with the following columns:
#'   - sample name
#'   - variant type (e.g. SNV, SV, CNV)
#'   - file label (e.g. ensemble, manta, vardict etc.)
#'   - run1 file path
#'   - run2 file path
#'
#' @examples
#' \dontrun{
#' final1 <- "path/to/bcbio/final1"
#' final2 <- "path/to/bcbio/final2"
#' merge_bcbio_outputs(final1, final2)
#' }
#' @export
merge_bcbio_outputs <- function(d1, d2, sample) {

  final1 <- bcbio_outputs(d1)
  final2 <- bcbio_outputs(d2)

  dplyr::inner_join(final1, final2, by = c("flabel", "vartype")) %>%
    dplyr::filter(!.data$vartype == "IGNORE_ME") %>%
    dplyr::mutate(sample_nm = sample) %>%
    dplyr::select(.data$sample_nm, .data$vartype, .data$flabel, .data$fpath.x, .data$fpath.y) %>%
    utils::write.table(file = "", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}

#' Get umccrise final output files
#'
#' Generates a tibble containing absolute paths to umccrise final files.
#'
#' @param d Path to `<umccrised/sample>` umccrise directory.
#' @return A tibble with the following columns:
#'   - vartype: variant type. Can be one of:
#'     - SNV (single-nucleotide/indel variants)
#'     - SV (structural variants)
#'     - CNV (copy number variants)
#'   - flabel: file label (e.g. pcgr, cpsr, purple etc.)
#'   - fpath: path to file
#'
#' @examples
#' \dontrun{
#' um <- "path/to/umccrised/sample"
#' umccrise_outputs(um)
#' }
#' @export
umccrise_outputs <- function(d) {
  stopifnot(dir.exists(d))
  # grab PURPLE's purple.gene.cnv file too
  vcfs <- list.files(d, pattern = "\\.vcf.gz$", recursive = TRUE, full.names = TRUE)
  purple_cnv <- list.files(d, pattern = "purple\\.gene", recursive = TRUE, full.names = TRUE)
  stopifnot(length(vcfs) > 0, length(purple_cnv) <= 1)
  all_files <- c(vcfs, purple_cnv)

  tibble::tibble(fpath = all_files) %>%
    dplyr::mutate(
      bname = sub("\\.vcf.gz$", "", basename(.data$fpath)),
      fpath = normalizePath(.data$fpath),
      flabel = dplyr::case_when(
        grepl("-somatic-ensemble-PASS$", .data$bname) ~ "pcgr_um",
        grepl("-normal-ensemble-predispose_genes$", .data$bname) ~ "cpsr_um",
        grepl("manta$", .data$bname) ~ "manta_um",
        grepl("purple\\.gene", .data$bname) ~ "purple-gene_um",
        TRUE ~ "IGNORE_ME"),
      vartype = dplyr::case_when(
        flabel == "IGNORE_ME" ~ "IGNORE_ME",
        flabel == "manta_um" ~ "SV",
        flabel == "purple-gene_um" ~ "CNV",
        flabel %in% c("pcgr_um", "cpsr_um") ~ "SNV",
        TRUE ~ "IGNORE_ME")) %>%
    dplyr::select(.data$vartype, .data$flabel, .data$fpath)
}

#' Gather umccrise filepaths from two umccrise final directories into a single tibble
#'
#' Generates a tibble containing absolute paths to (common) files from two umccrise final directories.
#'
#' @param d1 Path to first <umccrised/sample> directory.
#' @param d2 Path to second <umccrised/sample> directory.
#' @param sample Sample name.
#' @return A tibble with the following columns:
#'   - sample name
#'   - variant type (e.g. SNV, SV, CNV)
#'   - file label (e.g. pcgr, cpsr, purple etc.)
#'   - run1 file path
#'   - run2 file path
#'
#' @examples
#' \dontrun{
#' um1 <- "path/to/umccrised/sample1"
#' um2 <- "path/to/umccrised/sample2"
#' merge_umccrise_outputs(um1, um2)
#' }
#' @export
merge_umccrise_outputs <- function(d1, d2, sample) {

  um1 <- umccrise_outputs(d1)
  um2 <- umccrise_outputs(d2)

  dplyr::inner_join(um1, um2, by = c("flabel", "vartype")) %>%
    dplyr::filter(!.data$vartype == "IGNORE_ME") %>%
    dplyr::mutate(sample_nm = sample) %>%
    dplyr::select(.data$sample_nm, .data$vartype, .data$flabel, .data$fpath.x, .data$fpath.y) %>%
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

#' Read SNV count file output by woof
#'
#' Reads SNV count file output by woof
#'
#' @param x Path to file with counts
#' @return A single integer
#'
#' @examples
#'
#' x <- system.file("extdata", "count_vars.txt", package = "woofr")
#' read_count_file(x)
#'
#' @export
read_count_file <- function(x) {
  if (!file.exists(x)) {
    return(NA_integer_)
  }
  res <- readr::read_lines(x) %>% as.integer()
  stopifnot(res >= 0)
  res
}

#' Read eval file output by woof
#'
#' Reads eval file output by woof
#'
#' @param x Path to file with eval results
#' @return A tibble with a single row of evaluation metrics
#'
#' @examples
#'
#' x <- system.file("extdata", "eval_stats.tsv", package = "woofr")
#' read_eval_file(x)
#'
#' @export
read_eval_file <- function(x) {
  column_nms <- c("SNP_Truth", "SNP_TP", "SNP_FP", "SNP_FN", "SNP_Recall", "SNP_Precision",
                  "SNP_f1", "SNP_f2", "SNP_f3", "IND_Truth", "IND_TP", "IND_FP",
                  "IND_FN", "IND_Recall", "IND_Precision", "IND_f1", "IND_f2", "IND_f3")
  if (!file.exists(x)) {
    # return tibble of NAs
    res <- rep(NA, length(column_nms)) %>%
      purrr::set_names(column_nms) %>%
      as.list() %>%
      dplyr::as_tibble()

    return(res)
  }
  res <- readr::read_tsv(x, col_types = readr::cols(.default = "d"))
  stopifnot(names(res) == column_nms)
  res
}

#' Intersect two Manta VCF files
#'
#' Intersects two Manta VCF files for evaluation.
#'
#' @param f1 Path to first Manta file
#' @param f2 Path to second ('truthset') Manta file
#' @param bnd_switch Logical. Switch BND pairs for more matches (default: TRUE).
#' @return A list with the following elements:
#'   - tot_vars: total variants for file1 and file2
#'   - fp: tibble with False Positive calls i.e. variants in f1 that are not in f2
#'   - fn: tibble with False Negative calls i.e. variants in f2 that are not in f1
#'
#' @examples
#'
#' \dontrun{
#' f1 <- "path/to/run1/manta.vcf.gz"
#' f2 <- "path/to/run2/manta.vcf.gz"
#' mi <- manta_isec(f1, f2)
#' }
#'
#' @export
manta_isec <- function(f1, f2, bnd_switch = TRUE) {

  read_manta_both <- function(f1, f2) {
    vcf1 <- rock::prep_manta_vcf(f1, filter_pass = TRUE)$sv
    vcf2 <- rock::prep_manta_vcf(f2, filter_pass = TRUE)$sv
    list(f1 = vcf1, f2 = vcf2)
  }

  switch_bnd <- function(d) {
    stopifnot(all(colnames(d) %in% c("chrom1", "pos1", "chrom2", "pos2", "svtype")))
    no_bnd <- dplyr::filter(d, !.data$svtype %in% "BND")
    bnd <-
      dplyr::filter(d, .data$svtype %in% "BND") %>%
      dplyr::select(chrom1 = .data$chrom2, pos1 = .data$pos2,
                    chrom2 = .data$chrom1, pos2 = .data$pos1, .data$svtype)

    dplyr::bind_rows(no_bnd, bnd)
  }

  mb <- read_manta_both(f1, f2)
  tot_vars <- list(run1 = nrow(mb$f1), run2 = nrow(mb$f2))
  col_nms <- colnames(mb$f1)
  fp <- dplyr::anti_join(mb$f1, mb$f2, by = col_nms)
  fn <- dplyr::anti_join(mb$f2, mb$f1, by = col_nms)

  if (bnd_switch) {
    # some real matching cases simply have switched BND mates
    fp_new <- dplyr::anti_join(switch_bnd(fp), fn, by = col_nms) %>% switch_bnd()
    fn_new <- dplyr::anti_join(switch_bnd(fn), fp, by = col_nms) %>% switch_bnd()
    fp <- fp_new
    fn <- fn_new
  }

  return(list(tot_vars = tot_vars,
              fp = fp,
              fn = fn))
}
