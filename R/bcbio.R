# Sandbox for trying out things
require(tidyverse)
require(yaml)

# Given a bcbio config directory, read in <datetimestamp_project_batch-merged.yaml>.
# - 'details' contains X samples
# - each sample contains:
#   - algorithm (aligner, svcaller, variantcaller, fusion_caller)
#   - analysis (variant2, RNA-seq)
#   - description (sample name)
#   - genome_build (GRCh37, hg38)
#   - metadata (batch, phenotype, run)
# - 'upload': ../path/to/final


x <- "nogit/data/2019-02-01T0241_Cromwell_WGS_CUP-Pairs8-merged.yaml"
stopifnot(file.exists(x))

bcbio_sample <- function(s) {
  # given a subelement from 'details', extract stuff

  get_varcallers <- function(samp) {
    snv <- samp[["algorithm"]][["variantcaller"]]
    stopifnot(names(snv) %in% c("germline", "somatic"),
              length(snv) == 2,
              all(sapply(snv, length) == 3))

    snv_and_sv <- tibble::as_tibble(snv) %>%
      tidyr::gather(key = "caller_type") %>%
      dplyr::mutate(caller_type = sub("germline", "germ-snv", caller_type),
                    caller_type = sub("somatic", "soma-snv", caller_type)) %>%
      dplyr::bind_rows(
        tibble::tribble(
          ~caller_type, ~value,
          "soma-sv", samp[["algorithm"]][["svcaller"]]))

    snv_and_sv
  }

  algo <- s[["algorithm"]]
  analysis <- s[["analysis"]]
  nm <- s[["description"]]
  meta <- s[["metadata"]]
  varcallers <- NULL
  if (analysis == "variant2") {
    varcallers <- get_varcallers(s)
  }

  l <- list(sample_name = nm,
            batch = meta[["batch"]],
            phenotype = meta[["phenotype"]],
            analysis = analysis,
            genome_build = s[["genome_build"]],
            aligner = algo[["aligner"]],
            varcallers = varcallers,
            fusioncaller = algo[["fusion_caller"]])
  setNames(list(l), nm)
}

bcbio_batch <- function(sl) {
  # given a T/N sample list, output batch info

  stopifnot(length(sl) == 2)

  d <- purrr::map(sl, function(s) {
    tibble::tribble(
      ~sample_name, ~batch, ~phenotype, ~analysis, ~aligner, ~genome_build, ~varcallers,
      s$sample_name, s$batch, s$phenotype, s$analysis, s$aligner, s$genome_build, s$varcallers,
    )}) %>%
    dplyr::bind_rows()

  stopifnot(identical(d[1, c("batch", "analysis", "genome_build", "aligner", "varcallers")],
                      d[2, c("batch", "analysis", "genome_build", "aligner", "varcallers")]))

  b <- list()
  b$batch_name <- d$batch[1]
  b$tumor_name <- d$sample_name[d$phenotype == "tumor"]
  b$normal_name <- d$sample_name[d$phenotype == "normal"]
  b$analysis_type <- d$analysis[1]
  b$genome_build <- d$genome_build[1]
  b$aligner <- d$aligner[1]
  b$varcallers <- d$varcallers[[1]]

  b
}

bcbio_vcfs <- function(batch) {
  # Need batch/tumor/normal names, final path
  # Construct paths to VCFs
  bn <- batch[["batch_name"]]
  nn <- batch[["normal_name"]]
  tn <- batch[["tumor_name"]]
  fd <- batch[["final_dir"]]
  dd <- batch[["date_dir"]]
  vc <- batch[["varcallers"]]

  # if germ-snv: <final>/<date_dir>/<normal>-germline-<caller>-annotated.vcf.gz
  # if soma-snv: <final>/<date_dir>/<batch>-<caller>-annotated.vcf.gz
  # if soma-sv:  <final>/<tumor>/<batch>-sv-prioritize-manta.vcf.gz
  vc %>%
    dplyr::mutate(
      fpath = dplyr::case_when(
        caller_type == "germ-snv" ~ file.path(dd, paste0(nn, "-germline-", value, "-annotated.vcf.gz")),
        caller_type == "soma-snv" ~ file.path(dd, paste0(bn, "-", value, "-annotated.vcf.gz")),
        caller_type == "soma-sv" ~ file.path(fd, tn, paste0(bn, "-sv-prioritize-", value, ".vcf.gz")),
        TRUE ~ "XXX"))
}

read_bcbio_config <- function(x) {

  conf <- yaml::read_yaml(x)
  final_dir <- "/Users/pdiakumis/Desktop/projects/umccr/woof/nogit/data/f1/final"
  # final_dir <- normalizePath(conf[["upload"]][["dir"]], mustWork = F)
  date_dir <- Sys.glob(file.path(final_dir, paste0("*", conf[["fc_name"]])))
  samples <- purrr::map(conf[["details"]], function(s) bcbio_sample(s)[[1]]) %>%
    purrr::set_names(purrr::map_chr(., "sample_name"))
  batch <- bcbio_batch(samples)
  batch$final_dir <- final_dir
  batch$date_dir <- date_dir
  batch$varcallers <- bcbio_vcfs(batch)
  batch
}

batch <- read_bcbio_config(x)
