#' umccrise QC Summary Comparison
#'
#'
#' f1 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.0.0/umccrised/SEQC50__SEQC-II_tumour/cancer_report_tables/SEQC50__SEQC-II_tumour-qc_summary.tsv.gz"
#' f2 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/SEQC50__SEQC-II_tumour/cancer_report_tables/SEQC50__SEQC-II_tumour-qc_summary.tsv.gz"
#'
#' @param f1 Path to summary table for run1.
#' @param f2 Path to summary table for run2.
#' @param out Path to write results to.
#'
#' @export
qc_sum_cmp <- function(f1, f2, out) {
  col <- c("variable", "value")
  lab1 <- "run1"
  lab2 <- "run2"

  d1 <- readr::read_tsv(f1, col_select = dplyr::all_of(col), col_types = "cc") |>
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$value)
  d2 <- readr::read_tsv(f2, col_select = dplyr::all_of(col), col_types = "cc") |>
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$value)
  list(d1, d2) |>
    purrr::set_names(c(lab1, lab2)) |>
    dplyr::bind_rows(.id = "label") |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(cols = 2:tidyselect::last_col()) |>
    tidyr::pivot_wider(names_from = .data$label, values_from = .data$value) |>
    dplyr::mutate(equal = .data[[lab1]] %in% .data[[lab2]]) |>
    readr::write_tsv(out)
}
