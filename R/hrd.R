#' HRDetect/CHORD Comparison
#'
#' @param tool hrdetect or chord?
#' @param f1 Path to file1.
#' @param f2 Path to file2.
#' @param out Path to output file with results.
#'
#' @export
hrd_cmp <- function(tool, f1, f2, out) {
  # only interested in Probability/p_hrd for HRDetect/CHORD
  assertthat::assert_that(tool %in% c("hrdetect", "chord"), length(tool) == 1)
  col <- c( "hrdetect" = "Probability", "chord" = "p_hrd")
  p1 <- readr::read_tsv(f1, col_select = dplyr::all_of(col[tool]), col_types = "d") |> dplyr::pull()
  p2 <- readr::read_tsv(f2, col_select = dplyr::all_of(col[tool]), col_types = "d") |> dplyr::pull()
  tibble::tibble(
    run1 = p1,
    run2 = p2
  ) |>
    readr::write_tsv(out)
}
