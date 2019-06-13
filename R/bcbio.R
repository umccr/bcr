# Sandbox for trying out things
require(tidyverse)
require(yaml)

# Given a bcbio config directory, read in <datetimestamp_project_batch-merged.yaml>.
# - 'details' contains two samples
# - each sample contains:
#   - algorithm (aligner, svcaller, variantcaller, fusion_caller)
#   - analysis (variant2, RNA-seq)
#   - description (sample name)
#   - genome_build (GRCh37, hg38)
#   - metadata (batch, phenotype, run)
# - 'upload': ../path/to/final


x <- "nogit/data/2019-02-01T0241_Cromwell_WGS_CUP-Pairs8-merged.yaml"
# x <- "nogit/data/2019-05-31T1359_kym_WTS-merged.yaml"
stopifnot(file.exists(x))

read_bcbio_config <- function(x) {


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

  bcbio_sample <- function(s) {
    # given a subelement from 'details', extract stuff

    # s <- samples[[1]]
    algo <- s[["algorithm"]]
    analysis <- s[["analysis"]]
    nm <- s[["description"]]
    meta <- s[["metadata"]]
    varcallers <- NULL
    if (analysis == "variant2") {
      varcallers <- get_varcallers(s)
    }
    # if RNA: fusion_caller
    # if DNA: variantcaller, svcaller
    structure(
      list(
        sample_name = nm,
        batch = meta[["batch"]],
        phenotype = meta[["phenotype"]],
        analysis = analysis,
        aligner = algo[["aligner"]],
        varcallers = varcallers,
        fusioncaller = algo[["fusion_caller"]]),
      class = "bcbio_sample"
    )
  }

  bcbio_batch <- function(sl) {
    # given a sample list, output a list of batches with
    # tumor/normal pairs. e.g., for a file with
    # 10 T/N pairs, output a list of 10 batches

    # - gather elements with the same batch name


  }

  conf <- yaml::read_yaml(x)

  res <- list()
  samples <- conf$details
  res$n_samples <- length(samples)

  final_dir <- normalizePath(conf$upload$dir, mustWork = F)
  lapply(samples, bcbio_sample)

}
