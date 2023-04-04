
#' PCGR TSV Comparison
#'
#' @param f1 Path to file1.
#' @param f2 Path to file2.
#'
#' @examples
#'
#' \dontrun{
#' f1 <- "~/projects/woof/woof_compare_reports/nogit/umccrise_2.3.0rc4/umccrised/SEQC50__SEQC-II_tumour/small_variants/SEQC50__SEQC-II_tumour-somatic.pcgr.snvs_indels.tiers.tsv"
#' f2 <- "~/projects/woof/woof_compare_reports/nogit/umccrise_2.2.0/umccrised/SEQC50__SEQC-II_tumour/small_variants/SEQC50__SEQC-II_tumour-somatic.pcgr.snvs_indels.tiers.tsv"
#' pcgr_cmp(f1, f2)
#' }
#'
#' @export
pcgr_cmp <- function(f1, f2) {
  p1 <- readr::read_tsv(f1, col_types = readr::cols(.default = "c"))
  p2 <- readr::read_tsv(f2, col_types = readr::cols(.default = "c"))

  col_sel <- c("GENOMIC_CHANGE", "TIER", "SYMBOL", "CONSEQUENCE")
  # in one, not the other
  fp <- p1[!p1$GENOMIC_CHANGE %in% p2$GENOMIC_CHANGE, col_sel]
  fn <- p2[!p2$GENOMIC_CHANGE %in% p1$GENOMIC_CHANGE, col_sel]

  # differing tier/symbol/csq
  diff1 <- dplyr::anti_join(p1, p2, by = col_sel) |>
    dplyr::select(dplyr::all_of(col_sel)) |>
    dplyr::left_join(p2 |> dplyr::select(dplyr::all_of(col_sel)), by = "GENOMIC_CHANGE", suffix = c(".run1", ".run2")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      symbol_same = .data$SYMBOL.run1 %in% .data$SYMBOL.run2,
      tier_same = .data$TIER.run1 %in% .data$TIER.run2,
      csq_same = .data$CONSEQUENCE.run1 %in% .data$CONSEQUENCE.run2
    ) |>
    dplyr::ungroup() |>
    dplyr::select("GENOMIC_CHANGE", "symbol_same", "tier_same", "csq_same", "SYMBOL.run1", "SYMBOL.run2", "TIER.run1", "TIER.run2", "CONSEQUENCE.run1", "CONSEQUENCE.run2")

  list(
    tot1 = nrow(p1),
    tot2 = nrow(p2),
    fp = fp,
    fn = fn,
    diff = diff1
  )
}
