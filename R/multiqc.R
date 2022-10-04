x1 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/SEQC50/html/multiqc_data.json"
x2 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.0.0/SEQC50/html/multiqc_data.json"
lab1 <- "v212"
lab2 <- "v200"

#' Compare MultiQC JSONs
#'
#' Compares MultiQC JSON outputs from two runs.
#'
#' @param x1 Path to MultiQC JSON from run A.
#' @param x2 Path to MultiQC JSON from run B.
#' @param lab1 Label for run A.
#' @param lab2 Label for run B.
#'
#' @return Tibble with MultiQC metrics that differ between the two runs.
#' @export
multiqc_compare <- function(x1, x2, lab1, lab2) {
  # these should be 2-row tibbles
  d1 <- dracarys::multiqc_tidy_json(x1)
  d2 <- dracarys::multiqc_tidy_json(x2)
  id1 <- d1$umccr_id
  id2 <- d2$umccr_id
  assertthat::assert_that(length(id1) == length(id2), length(id1) == 2, all(id1 %in% id2))
  nm <- list(
    lab1_a = glue::glue("{lab1}_{id1[1]}"),
    lab1_b = glue::glue("{lab1}_{id1[2]}"),
    lab2_a = glue::glue("{lab2}_{id1[1]}"),
    lab2_b = glue::glue("{lab2}_{id1[2]}")
  )
  d <- list(d1, d2) |>
    purrr::set_names(c(lab1, lab2)) |>
    dplyr::bind_rows(.id = "label") |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character)) |>
    dplyr::select(.data$umccr_id, .data$label, dplyr::everything()) |>
    tidyr::pivot_longer(cols = 3:tidyselect::last_col()) |>
    tidyr::pivot_wider(names_from = c(.data$label, .data$umccr_id), values_from = .data$value) |>
    dplyr::mutate(equal1 = .data[[nm$lab1_a]] %in% .data[[nm$lab2_a]],
                  equal2 = .data[[nm$lab1_b]] %in% .data[[nm$lab2_b]])

  d |> dplyr::filter(!.data$equal1 | !.data$equal2)
}

