#' Compare MultiQC JSONs
#'
#' Compares MultiQC JSON outputs from two runs.
#'
#' @param x1 Path to MultiQC JSON from run A.
#' @param x2 Path to MultiQC JSON from run B.
#' @param out Path to output results.
#'
#' @return Tibble with MultiQC metrics that differ between the two runs.
#'
#' @examples
#' \dontrun{
#' x1 <- "multiqc_data1.json"
#' x2 <- "multiqc_data2.json"
#' lab1 <- "v212"
#' lab2 <- "v200"
#' cmp(x1, x2, lab1, lab2)
#'}
#' @export
multiqc_cmp <- function(x1, x2, out) {
  # these should be 2-row tibbles for umccrise
  # but might be 1-row for others
  d1 <- dracarys::multiqc_tidy_json(x1)
  d2 <- dracarys::multiqc_tidy_json(x2)
  id1 <- d1$umccr_id
  id2 <- d2$umccr_id
  assertthat::assert_that(length(id1) == length(id2), length(id1) %in% c(1, 2), all(id1 %in% id2))
  sd <- letters[1:nrow(d1)] |> purrr::set_names(id1)
  lab1 <- "run1"
  lab2 <- "run2"
  d <- list(d1, d2) |>
    purrr::set_names(c(lab1, lab2)) |>
    dplyr::bind_rows(.id = "label") |>
    dplyr::mutate(woofr_sample = sd[.data$umccr_id]) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character)) |>
    dplyr::select("label", "woofr_sample", dplyr::everything()) |>
    tidyr::pivot_longer(cols = 3:tidyselect::last_col()) |>
    tidyr::pivot_wider(names_from = c(.data$label, .data$woofr_sample), values_from = .data$value) |>
    dplyr::mutate(equal1 = .data$run1_a %in% .data$run2_a)

  if (length(id1) == 2) {
    d <- d |>
      dplyr::mutate(equal2 = .data$run1_b %in% .data$run2_b) |>
      dplyr::filter(!.data$equal1 | !.data$equal2)
  } else {
    d <- d |>
      dplyr::filter(!.data$equal1)
  }
  readr::write_tsv(d, out)
}
