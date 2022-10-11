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
  purple_cnv <- list.files(d, pattern = "purple.*gene", recursive = TRUE, full.names = TRUE)
  stopifnot(length(vcfs) > 0, length(purple_cnv) <= 1)
  all_files <- c(vcfs, purple_cnv)

  tibble::tibble(fpath = all_files) %>%
    dplyr::mutate(
      bname = sub("\\.vcf.gz$", "", basename(.data$fpath)),
      fpath = normalizePath(.data$fpath),
      flabel = dplyr::case_when(
        grepl("-somatic-PASS$", .data$bname) ~ "som",
        grepl("germline.predispose_genes$", .data$bname) ~ "germ",
        grepl("manta$", .data$bname) ~ "manta",
        grepl("purple.*gene", .data$bname) ~ "purple_gene",
        grepl("sage", .data$bname) ~ "sage",
        TRUE ~ "IGNORE_ME"),
      vartype = dplyr::case_when(
        flabel == "IGNORE_ME" ~ "IGNORE_ME",
        flabel == "manta" ~ "SV",
        flabel == "purple_gene" ~ "CNV",
        flabel %in% c("som", "germ", "sage") ~ "SNV",
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


#' Read SNV count file output by woof
#'
#' Reads SNV count file output by woof
#'
#' @param x Path to file with counts
#' @return A single integer
#'
#' @examples
#'
#' x <- system.file("extdata", "snv/count_vars.txt", package = "woofr")
#' read_snv_count_file(x)
#'
#' @export
read_snv_count_file <- function(x) {
  column_nms <- c("sample", "flabel", "count")
  if (!file.exists(x)) {
    # return tibble of NAs
    res <- rep(NA, length(column_nms)) %>%
      purrr::set_names(column_nms) %>%
      as.list() %>%
      dplyr::as_tibble()

    return(res)
  }
  res <- readr::read_tsv(x, col_types = "cci")
  stopifnot(names(res) == column_nms)
  res
}

#' Read SNV eval file output by woof
#'
#' Reads SNV eval file output by woof
#'
#' @param x Path to file with eval results
#' @return A tibble with a single row of evaluation metrics
#'
#' @examples
#'
#' x <- system.file("extdata", "snv/eval_stats.tsv", package = "woofr")
#' read_snv_eval_file(x)
#'
#' @export
read_snv_eval_file <- function(x) {
  column_nms <- c("sample", "flabel", "subset",
                  "SNP_Truth", "SNP_TP", "SNP_FP", "SNP_FN", "SNP_Recall", "SNP_Precision",
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
  res <- readr::read_tsv(x, col_types = readr::cols(
    .default = "d", sample = "c", flabel = "c", subset = "c"))
  stopifnot(names(res) == column_nms)
  res
}

#' Read SV FP/FN file output by woof
#'
#' Reads SV FP/FN file output by woof
#'
#' @param x Path to file with FP/FN variants
#' @return A tibble containing FP/FN SV variants
#'
#' @examples
#'
#' x <- system.file("extdata", "sv/fpfn.tsv", package = "woofr")
#' read_sv_fpfn_file(x)
#'
#' @export
read_sv_fpfn_file <- function(x) {
  column_nms <- c("FP_or_FN", "sample", "flabel", "chrom1","pos1", "chrom2", "pos2", "svtype", "af", "fmt_map", "fmt_val")
  if (!file.exists(x)) {
    res <- rep(NA, length(column_nms)) %>%
      purrr::set_names(column_nms) %>%
      as.list() %>%
      dplyr::as_tibble()
    return(res)
  }
  res <- readr::read_tsv(x, col_types = "ccccicicccc")
  stopifnot(names(res) == column_nms)
  res |>
    tidyr::separate(.data$fmt_val, into = c("PR_REF", "PR_ALT", "SR_REF", "SR_ALT"), convert = TRUE) |>
    dplyr::mutate(`SR>PR` = SR_ALT > PR_ALT) |>
    dplyr::select(-c(.data$PR_REF, .data$SR_REF))
}

#' Read SV eval metrics file output by woof
#'
#' Reads SV eval metrics file output by woof
#'
#' @param x Path to file with SV evaluation metrics
#' @return A tibble with a single row of evaluation metrics
#'
#' @examples
#'
#' x <- system.file("extdata", "sv/eval_metrics.tsv", package = "woofr")
#' read_sv_eval_file(x)
#'
#' @export
read_sv_eval_file <- function(x) {
  column_nms <- c("sample", "flabel", "run1_count", "run2_count", "Recall",
                  "Precision", "Truth", "TP", "FP", "FN")
  if (!file.exists(x)) {
    res <- rep(NA, length(column_nms)) %>%
      purrr::set_names(column_nms) %>%
      as.list() %>%
      dplyr::as_tibble()
    return(res)
  }
  res <- readr::read_tsv(x, col_types = "cciiddiiii")
  stopifnot(names(res) == column_nms)
  res
}


#' Intersect two Manta VCF files
#'
#' Intersects two Manta VCF files for evaluation.
#'
#' @param f1 Path to first Manta file
#' @param f2 Path to second ('truthset') Manta file
#' @param samplename Sample name (used for labelling).
#' @param flab File name (used for labelling).
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
manta_isec <- function(f1, f2, samplename, flab, bnd_switch = TRUE) {

  read_manta_both <- function(f1, f2) {
    vcf1 <- prep_manta_vcf(f1, filter_pass = TRUE)$sv
    vcf2 <- prep_manta_vcf(f2, filter_pass = TRUE)$sv
    list(f1 = vcf1, f2 = vcf2)
  }

  switch_bnd <- function(d) {
    stopifnot(all(colnames(d) %in% c("chrom1", "pos1", "chrom2", "pos2", "svtype", "af", "fmt_map", "fmt_val")))
    no_bnd <- dplyr::filter(d, !.data$svtype %in% "BND")
    bnd <-
      dplyr::filter(d, .data$svtype %in% "BND") %>%
      dplyr::select(chrom1 = .data$chrom2, pos1 = .data$pos2,
                    chrom2 = .data$chrom1, pos2 = .data$pos1, .data$svtype,
                    .data$af, .data$fmt_map, .data$fmt_val)

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

  fp <- fp %>%
    dplyr::mutate(sample = samplename, flabel = flab) %>%
    dplyr::select(.data$sample, .data$flabel, dplyr::everything())
  fn <- fn %>%
    dplyr::mutate(sample = samplename, flabel = flab) %>%
    dplyr::select(.data$sample, .data$flabel, dplyr::everything())

  return(list(tot_vars = tot_vars,
              fp = fp,
              fn = fn))
}

#' Manta Comparison Metrics
#'
#' Returns evaluation metrics from comparing two Manta call sets.
#'
#' @param mi Output from \code{\link{manta_isec}}.
#' @param samplename Sample name (used for labelling).
#' @param flab File name (used for labelling).
#' @return A single row tibble with the following columns:
#'   - sample: samplename
#'   - flabel: flab
#'   - run1_count: total variants in first file
#'   - run2_count: total variants in second file
#'   - TP: in first and second
#'   - FP: in first but not second
#'   - FN: in second but not first
#'   - Recall: TP / (TP + FN)
#'   - Precision: TP / (TP + FP)
#'   - Truth: TP + FN
#'
#' @examples
#'
#' \dontrun{
#' f1 <- "path/to/run1/manta.vcf.gz"
#' f2 <- "path/to/run2/manta.vcf.gz"
#' mi <- manta_isec(f1, f2, "sampleA", "manta1")
#' manta_isec_stats(mi, "sampleA", "manta1")
#' }
#'
#' @export
manta_isec_stats <- function(mi, samplename, flab) {
  fn <- nrow(mi$fn)
  fp <- nrow(mi$fp)
  tp <- mi$tot_vars$run2 - fn
  truth <- tp + fn
  recall <- tp / truth
  precision <- tp / (tp + fp)

  dplyr::tibble(
    sample = samplename, flabel = flab,
    run1_count = mi$tot_vars$run1, run2_count = mi$tot_vars$run2,
    Recall = round(recall, 3), Precision = round(precision, 3), Truth = truth,
    TP = tp, FP = fp, FN = fn
  )
}

#' Get Manta Circos with FP/FN calls
#'
#' Returns Circos plot highlighting FP/FN calls from a Manta comparison.
#'
#' @param mi Output from \code{\link{manta_isec}}.
#' @param samplename Sample name (used for labelling).
#' @param outdir Directory to write circos to.
#' @return Path to circos plot highlighting FP (green) and FN (red) calls from a
#'   Manta comparison.
#'
#' @examples
#'
#' \dontrun{
#' f1 <- "path/to/run1/manta.vcf.gz"
#' f2 <- "path/to/run2/manta.vcf.gz"
#' mi <- manta_isec(f1, f2)
#' get_circos(mi, "sampleA", "outdir1")
#' }
#'
#' @export
get_circos <- function(mi, samplename, outdir) {

  prep_svs_circos <- function() {
    sv <- dplyr::bind_rows(list(fp = mi$fp, fn = mi$fn), .id = "fp_or_fn")
    links_coloured <- sv %>%
      dplyr::mutate(
        chrom1 = paste0("hs", .data$chrom1),
        chrom2 = paste0("hs", .data$chrom2),
        col = dplyr::case_when(
          fp_or_fn == "fp" ~ '(0,255,0)',
          fp_or_fn == "fn" ~ '(255,0,0)',
          TRUE ~ 'Oops'),
        pos1b = .data$pos1,
        pos2b = .data$pos2,
        col = paste0('color=', col)) %>%
      dplyr::select(.data$chrom1, .data$pos1, .data$pos1b, .data$chrom2, .data$pos2, .data$pos2b, .data$col)

    return(links_coloured)
  }

  write_circos_configs <- function() {
    dir.create(outdir, recursive = TRUE)
    links <- prep_svs_circos()
    message(glue::glue("Writing circos links file to '{outdir}'."))
    readr::write_tsv(links, file.path(outdir, "SAMPLE.link.circos"), col_names = FALSE)

    message(glue::glue("Copying circos templates to '{outdir}'"))
    file.copy(system.file("extdata/circos/circos_sv.conf", package = "woofr"),
              file.path(outdir, "circos.conf"), overwrite = TRUE)
    file.copy(system.file("extdata/circos/gaps.txt", package = "woofr"),
              outdir, overwrite = TRUE)
    file.copy(system.file("extdata/circos/ideogram.conf", package = "woofr"),
              outdir, overwrite = TRUE)
  }

  run_circos <- function() {
    circos_png <- glue::glue("circos_{samplename}.png")
    cmd <- glue::glue("echo '$(date) running circos on {samplename}' && circos -nosvg -conf {outdir}/circos.conf -outputdir {outdir} -outputfile {circos_png}")

    if (Sys.which("circos") != "") {
      system(cmd, ignore.stdout = TRUE)
      return(circos_png)
    } else {
      stop("Can't find 'circos' in your PATH. Exiting.")
    }
  }

  write_circos_configs()
  circos_png <- run_circos()
  circos_png
}


#' Read PURPLE gene segments
#'
#' Reads the gene.cnv file output by PURPLE
#'
#' @param x Path to PURPLE `gene.cnv` (or `cnv.gene.tsv`) file.
#' @return A tibble with the main columns of interest from the PURPLE `gene.cnv` file i.e.
#'   chrom, start, end, gene, min_cn and max_cn.
#'
#' @examples
#'
#' x <- system.file("extdata", "cnv/sample_A.purple.gene.cnv", package = "woofr")
#' y <- system.file("extdata", "cnv/sample_B.purple.cnv.gene.tsv", package = "woofr")
#' read_purple_gene_file(x)
#' read_purple_gene_file(y)
#'
#' @export
read_purple_gene_file <- function(x) {
  column_nms <- c("chromosome", "start", "end", "gene", "mincopynumber", "maxcopynumber")
  d <- readr::read_tsv(x, col_types = readr::cols(.default = "c")) %>%
    dplyr::select(1:6)
  names(d) <- tolower(names(d))
  stopifnot(names(d) == column_nms)
  d %>%
    dplyr::mutate(
      start = as.integer(.data$start),
      end = as.integer(.data$end),
      min_cn = round(as.numeric(.data$mincopynumber), 1),
      max_cn = round(as.numeric(.data$maxcopynumber), 1)) %>%
    dplyr::select(
      chrom = .data$chromosome, .data$start, .data$end,.data$gene, .data$min_cn, .data$max_cn)
}

#' Compare two PURPLE gene CNV files
#'
#' Compares two PURPLE `gene.cnv` (or `cnv.gene.tsv`) files.
#'
#' @param cnv1 Path to first PURPLE `gene.cnv` (or `cnv.gene.tsv`) file.
#' @param cnv2 Path to second PURPLE `gene.cnv` (or `cnv.gene.tsv`) file ('TRUTHSET').
#' @param out_cn_diff Path to write genes with copy number differences between
#'   the two runs - needs to be a writable directory.
#' @param out_coord_diff Path to write genes with different coordinates between
#'   the two runs - needs to be a writable directory.
#' @param threshold Numeric. Copy number differences smaller than this value are not reported.
#' @return Writes results from the comparison to `out_cn_diff` and `out_coord_diff`.
#'
#'
#' @examples
#'
#' cnv1 <- system.file("extdata", "cnv/sample_A.purple.gene.cnv", package = "woofr")
#' cnv2 <- system.file("extdata", "cnv/sample_B.purple.cnv.gene.tsv", package = "woofr")
#' out1 <- tempfile()
#' out2 <- tempfile()
#' compare_purple_gene_files(cnv1, cnv2, out1, out2, threshold = 0.1)
#'
#' @export
compare_purple_gene_files <- function(cnv1, cnv2, out_cn_diff, out_coord_diff, threshold = 0.1) {
  stopifnot(is.numeric(threshold), threshold >= 0, length(threshold) == 1)

  x1 <- read_purple_gene_file(cnv1)
  x2 <- read_purple_gene_file(cnv2)
  # compare chrom,start,end,gene
  coord_diff <-
    dplyr::bind_rows(
      fp = dplyr::anti_join(x1, x2, by = c("chrom", "start", "end", "gene")),
      fn = dplyr::anti_join(x2, x1, by = c("chrom", "start", "end", "gene")),
      .id = "fp_or_fn")

  # compare min/max cn
  cn_diff <-
    dplyr::left_join(x1, x2, by = c("chrom", "start", "end", "gene"), suffix = paste0(".run", 1:2)) %>%
    dplyr::mutate(min_diff = abs(.data$min_cn.run1 - .data$min_cn.run2) > threshold,
                  max_diff = abs(.data$max_cn.run1 - .data$max_cn.run2) > threshold) %>%
    dplyr::filter(.data$min_diff | .data$max_diff)

  utils::write.table(cn_diff, file = out_cn_diff, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  utils::write.table(coord_diff, file = out_coord_diff, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
}

#' Check Comparison Input File
#'
#' Checks that the input file is suitable for running 'woof compare'.
#'
#' @param x Path to tab-separated comparison input file.
#'       Must contain the following five fields per file (NOTE: do not include column names):
#'       sample name, variant type (SNV, SV or CNV), file label, run1 & run2 file paths (can be S3 paths).
#' @return If the file has issues, an error message. If there
#' are no issues, simply returns file path.
#'
#' @examples
#' \dontrun{
#' x <- "path/to/cromwell_inputs.tsv"
#' check_comparison_input(x)
#' }
#' @export
check_comparison_input <- function(x) {
  d <- readr::read_tsv(x, col_types = readr::cols(.default = "c"),
                       col_names = c("sample", "vartype", "flabel", "run1", "run2"))
  assertthat::assert_that(
    all(d$vartype %in% c("SNV", "SV", "CNV")),
    all(startsWith(d$run1, "/") | startsWith(d$run1, "s3")),
    all(startsWith(d$run2, "/") | startsWith(d$run2, "s3"))
  )

  x
}

#' Read Manta VCF
#'
#' Read main columns of interest from Manta VCF using bcftools or bedr
#'
#' Uses bcftools (https://samtools.github.io/bcftools/bcftools.html)
#' or bedr (https://cran.r-project.org/web/packages/bedr/index.html) to read
#' in the VCF file.
#'
#' @param vcf Path to Manta VCF file (`.vcf.gz` or `.vcf`).
#' @return A dataframe (`tibble`) with the following fields from the VCF:
#'   * chrom1: `CHROM` (remove `chr` prefix (if any) for hg38 compatibility)
#'   * pos1: `POS` | `INFO/BPI_START`
#'   * pos2: `INFO/END` | `INFO/BPI_END`
#'   * id: `ID`
#'   * mateid: `INFO/MATEID`
#'   * svtype: `INFO/SVTYPE`
#'   * filter: `FILTER`
#'   * af: `BPI_AF`
#'   * fmt_map: `FORMAT`
#'   * fmt_val: `SAMPLE`
read_manta_vcf <- function(vcf) {

  stopifnot(file.exists(vcf), length(vcf) == 1)

  # You have two options: use bcftools (first choice) or bedr
  if (Sys.which("bcftools") == "") {
    # use bedr
    x <- bedr::read.vcf(vcf, split.info = TRUE, verbose = FALSE)
    DF <- tibble::tibble(chrom1 = as.character(x$vcf$CHROM),
                         pos1 = "dummy1",
                         pos2 = "dummy2",
                         id = x$vcf$ID,
                         mateid = x$vcf$MATEID,
                         svtype = x$vcf$SVTYPE,
                         filter = x$vcf$FILTER,
                         af = x$vcf$BPI_AF,
                         fmt_map = x$vcf$FORMAT,
                         fmt_val = x$vcf[[length(x$vcf)]])

    if (any(grepl("BPI_START", x$header$INFO[, "ID"]))) {
      # use BPI fields
      DF <- dplyr::mutate(DF,
                          pos1 = as.integer(x$vcf$BPI_START),
                          pos2 = as.integer(x$vcf$BPI_END))

    } else {
      # use typical fields
      DF <- dplyr::mutate(DF,
                          pos1 = as.integer(x$vcf$POS),
                          pos2 = as.integer(x$vcf$END))
    }

  } else {
    # use bcftools
    if (system(paste0("bcftools view -h ",  vcf, " | grep 'BPI_START'"), ignore.stdout = TRUE) == 0) {
      # use BPI fields
      bcftools_query <- "bcftools query -f '%CHROM\t%INFO/BPI_START\t%INFO/BPI_END\t%ID\t%INFO/MATEID\t%INFO/SVTYPE\t%FILTER\t%INFO/BPI_AF\t%FORMAT\n'"
    } else {
      # use typical fields, no BPI_AF --- TODO: FIX
      bcftools_query <- "bcftools query -f '%CHROM\t%POS\t%INFO/END\t%ID\t%INFO/MATEID\t%INFO/SVTYPE\t%FILTER\t%FORMAT\n'"
    }

    DF <- system(paste(bcftools_query, vcf), intern = TRUE) %>%
      tibble::tibble(all_cols = .) %>%
      tidyr::separate(col = .data$all_cols,
                      into = c("chrom1", "pos1", "pos2", "id", "mateid", "svtype", "filter", "af", "fmt_map", "fmt_val"),
                      sep = "\t", convert = TRUE) %>%
      dplyr::mutate(chrom1 = as.character(.data$chrom1))

  }

  DF %>%
    dplyr::mutate(chrom1 = as.character(sub("chr", "", .data$chrom1)),
                  pos1 = char2num(.data$pos1),
                  pos2 = char2num(.data$pos2)) # sometimes get '.' in pos2 (e.g. from purple)
}

#' Prepare Manta VCF for Circos
#'
#' Prepares a Manta VCF for display in a Circos plot.
#'
#' @param vcf Path to Manta VCF file. Can be compressed or not.
#' @param filter_pass Keep only variants annotated with a PASS FILTER?
#'   (default: FALSE).
#' @return A dataframe (`tibble`) with the following fields from the VCF:
#'   * chrom1: `CHROM`
#'   * pos1: `POS` | `INFO/BPI_START`
#'   * chrom2: `CHROM` (for mate2 if BND)
#'   * pos2: `INFO/END` | `INFO/BPI_END` (for mate1 if BND)
#'   * svtype: `INFO/SVTYPE`. Used for plotting.
#'
#' @export
prep_manta_vcf <- function(vcf, filter_pass = FALSE) {

  DF <- read_manta_vcf(vcf)

  # BNDs

  # We have POS of mate2 through INFO/END or INFO/BPI_END, just need CHROM.
  # If no BPI, Manta doesn't annotate BNDs with END. Let's grab it from the mate's POS.
  # Sometimes with post-processing a mate might get filtered out.
  # In these cases just filter out the orphan mate.
  #
  # Keep BND mate with index 1 (i.e. discard duplicated information)
  # see <https://github.com/Illumina/manta/blob/master/docs/developerGuide/ID.md>
  df_bnd1 <- DF %>%
    dplyr::filter(.data$svtype == "BND")

  match_id2mateid <- match(df_bnd1$id, df_bnd1$mateid)
  df_bnd2 <-
    df_bnd1[match_id2mateid, c("chrom1", "pos1")] %>%
    dplyr::rename(chrom11 = .data$chrom1,
                  pos11 = .data$pos1)
  df_bnd <-
    dplyr::bind_cols(df_bnd1, df_bnd2) %>%
    dplyr::rename(chrom2 = .data$chrom11) %>%
    dplyr::mutate(pos2 = ifelse(is.na(.data$pos2), .data$pos11, .data$pos2),
                  bndid = substring(.data$id, nchar(.data$id)))

  orphan_mates <- df_bnd %>%
    dplyr::filter(.data$chrom2 %in% NA) %>%
    dplyr::mutate(orphan = paste0(.data$chrom1, ":", .data$pos1)) %>%
    dplyr::pull(.data$orphan)

  df_bnd <- df_bnd %>%
    dplyr::filter(!is.na(.data$chrom2)) %>%
    dplyr::filter(.data$bndid == "1") %>%
    dplyr::select(.data$chrom1, .data$pos1, .data$chrom2,
                  .data$pos2, .data$id, .data$mateid, .data$svtype, .data$filter,
                  .data$af, .data$fmt_map, .data$fmt_val)

  if (length(orphan_mates) > 0) {
    warning(glue::glue("The following {length(orphan_mates)} orphan BND mates are removed:\n",
                       paste(orphan_mates, collapse = ", ")))
  }

  stopifnot(.manta_proper_pairs(df_bnd$id, df_bnd$mateid))

  # Non-BNDs
  df_other <- DF %>%
    dplyr::filter(.data$svtype != "BND") %>%
    dplyr::mutate(chrom2 = .data$chrom1) %>%
    dplyr::select(.data$chrom1, .data$pos1, .data$chrom2, .data$pos2,
                  .data$id, .data$mateid, .data$svtype, .data$filter,
                  .data$af, .data$fmt_map, .data$fmt_val)

  # All together now
  sv <- df_other %>%
    dplyr::bind_rows(df_bnd)

  if (filter_pass) {
    sv <- sv %>%
      dplyr::filter(.data$filter == "PASS")
  }

  sv <- sv %>%
    dplyr::select(.data$chrom1, .data$pos1,
                  .data$chrom2, .data$pos2, .data$svtype,
                  .data$af, .data$fmt_map, .data$fmt_val)

  structure(list(sv = sv), class = "sv")
}


# Check if Manta BND mates are properly paired
.manta_proper_pairs <- function(id, mid) {
  ext1 <- substring(id, nchar(id))
  ext2 <- substring(mid, nchar(mid))
  pre1 <- substring(id, 1, nchar(id) - 1)
  pre2 <- substring(mid, 1, nchar(mid) - 1)

  # id should end in 1; mateid in 0
  if (all(ext1 == "1") & all(ext2 == "0") & all(pre1 == pre2)) {
    return(TRUE)
  }
  return(FALSE)
}


