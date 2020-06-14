
bcbio_sample <- function(s) {
  # given a subelement from 'details', extract stuff

  get_varcallers <- function(samp) {
    snv <- samp[["algorithm"]][["variantcaller"]]
    stopifnot(names(snv) %in% c("germline", "somatic"),
              length(snv) == 2,
              length(snv[["germline"]]) == length(snv[["somatic"]]))

    snv_and_sv <- tibble::as_tibble(snv) %>%
      dplyr::bind_rows(tibble::tribble(~germline, ~somatic,
                                       "ensemble", "ensemble")) %>%
      tidyr::gather(key = "caller_type", "caller_name") %>%
      dplyr::mutate(caller_type = sub("germline", "ger", .data$caller_type),
                    caller_type = sub("somatic", "som", .data$caller_type)) %>%
      dplyr::bind_rows(
        tibble::tribble(
          ~caller_type, ~caller_name,
          "som_sv", samp[["algorithm"]][["svcaller"]])) %>%
      dplyr::mutate(caller_name2 = paste0(.data$caller_name, "_", .data$caller_type)) %>%
      dplyr::arrange(.data$caller_name)

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
  stats::setNames(list(l), nm)
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

# Construct paths to VCFs
bcbio_vcfs <- function(batch) {

  bn <- batch[["batch_name"]]
  nn <- batch[["normal_name"]]
  tn <- batch[["tumor_name"]]
  fd <- batch[["final_dir"]]
  dd <- batch[["date_dir"]]
  vc <- batch[["varcallers"]]

  # if germ-snv: <final>/<date_dir>/<normal>-germline-<caller>-annotated.vcf.gz
  # if soma-snv: <final>/<date_dir>/<batch>-<caller>-annotated.vcf.gz
  # if soma-sv:  <final>/<tumor>/<batch>-sv-prioritize-<caller>.vcf.gz
  # Also add ensemble calls
  vc %>%
    dplyr::mutate(
      fpath = dplyr::case_when(
        caller_type == "ger" ~ file.path(dd, paste0(nn, "-germline-", .data$caller_name, "-annotated.vcf.gz")),
        caller_type == "som" ~ file.path(dd, paste0(bn, "-", .data$caller_name, "-annotated.vcf.gz")),
        caller_type == "som_sv" ~ file.path(fd, tn, paste0(bn, "-sv-prioritize-", .data$caller_name, ".vcf.gz")),
        TRUE ~ "XXX"))
}

#' Read bcbio Config
#'
#' Generates a list of information about the batch within a bcbio
#' config YAML file.
#'
#' @param x Path to `bcbio/config/<datestamp>_project.yaml` file.
#' @return A list with the following elements:
#'   - 'batch_name', 'tumor_name' and 'normal_name': names of batch and its
#'     tumor/normal samples.
#'   - 'analysis_type': variant2 or RNA-seq
#'   - 'genome_build': GRCh37 or hg38
#'   - 'aligner': bwa, star etc.
#'
#' @examples
#' \dontrun{
#' x <- "path/to/bcbio/config/<datestamp>_project.yaml"
#' read_bcbio_config(x)
#' }
#' @export
read_bcbio_config <- function(x) {

  conf <- yaml::read_yaml(x)
  final_dir <- normalizePath(conf[["upload"]][["dir"]], mustWork = TRUE)
  date_dir <- Sys.glob(file.path(final_dir, paste0("*", conf[["fc_name"]])))

  samples <- purrr::map(conf[["details"]], function(s) bcbio_sample(s)[[1]])
  nms <- purrr::map_chr(samples, "sample_name")
  samples <- stats::setNames(samples, nms)

  batch <- bcbio_batch(samples)
  batch$final_dir <- final_dir
  batch$date_dir <- date_dir
  batch$varcallers <- bcbio_vcfs(batch)
  batch
}
