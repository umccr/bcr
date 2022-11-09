f1 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/SEQC50__SEQC-II_tumour/small_variants/SEQC50__SEQC-II_tumour-somatic.pcgr.snvs_indels.tiers.tsv"
f2 <- "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/SEQC50__SEQC-II_tumour/SEQC50__SEQC-II_tumour.pcgr.snvs_indels.tiers.tsv"

#' PCGR TSV Comparison
#'
#' @param f1 Path to file1.
#' @param f2 Path to file2.
#' @param out Dir to output results.
#'
#' @export
pcgr_cmp <- function(f1, f2) {
  p1 <- readr::read_tsv(f1, col_types = readr::cols(.default = "c"))
  p2 <- readr::read_tsv(f2, col_types = readr::cols(.default = "c"))

  col_sel <- c("GENOMIC_CHANGE", "TIER")
  # in one, not the other
  fp <- p1[!p1$GENOMIC_CHANGE %in% p2$GENOMIC_CHANGE, col_sel]
  fn <- p2[!p2$GENOMIC_CHANGE %in% p1$GENOMIC_CHANGE, col_sel]

  # differing tiers
  tiers_diff <- dplyr::anti_join(p1, p2, by = col_sel) |>
    dplyr::select(dplyr::all_of(col_sel)) |>
    dplyr::left_join(p2 |> dplyr::select(dplyr::all_of(col_sel)), by = "GENOMIC_CHANGE")

  list(
    tot1 = nrow(p1),
    tot2 = nrow(p2),
    fp = fp,
    fn = fn,
    tiers_diff = tiers_diff
  )
}

pcgr_cmp(f1, f2)


x <- tibble::tribble(
 ~sample, ~run1, ~run2,
 "B10", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/B_ALL_Case_10__MDX190025/B_ALL_Case_10__MDX190025.pcgr.snvs_indels.tiers.tsv",                    "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/B_ALL_Case_10__MDX190025/small_variants/B_ALL_Case_10__MDX190025-somatic.pcgr.snvs_indels.tiers.tsv",
 "DNR", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/DiploidNeverResponder__PRJ170218/DiploidNeverResponder__PRJ170218.pcgr.snvs_indels.tiers.tsv",    "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/DiploidNeverResponder__PRJ170218/small_variants/DiploidNeverResponder__PRJ170218-somatic.pcgr.snvs_indels.tiers.tsv",
 "P033", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/2016.249.17.MH.P033__CCR170115/2016.249.17.MH.P033__CCR170115.pcgr.snvs_indels.tiers.tsv",        "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/2016.249.17.MH.P033__CCR170115/small_variants/2016.249.17.MH.P033__CCR170115-somatic.pcgr.snvs_indels.tiers.tsv",
 "P025", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/2016.249.18.WH.P025__CCR180149/2016.249.18.WH.P025__CCR180149.pcgr.snvs_indels.tiers.tsv",        "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/2016.249.18.WH.P025__CCR180149/small_variants/2016.249.18.WH.P025__CCR180149-somatic.pcgr.snvs_indels.tiers.tsv",
 "SEQC", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/SEQC50__SEQC-II_tumour/SEQC50__SEQC-II_tumour.pcgr.snvs_indels.tiers.tsv",                        "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/SEQC50__SEQC-II_tumour/small_variants/SEQC50__SEQC-II_tumour-somatic.pcgr.snvs_indels.tiers.tsv",
 "SFRC", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/SFRC01073__PRJ180598/SFRC01073__PRJ180598.pcgr.snvs_indels.tiers.tsv",                            "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/SFRC01073__PRJ180598/small_variants/SFRC01073__PRJ180598-somatic.pcgr.snvs_indels.tiers.tsv",
 "SBJ303", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/SBJ00303__PRJ200200/SBJ00303__PRJ200200.pcgr.snvs_indels.tiers.tsv",                              "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/SBJ00303__PRJ200200/small_variants/SBJ00303__PRJ200200-somatic.pcgr.snvs_indels.tiers.tsv",
 "CUP8", "/Users/pdiakumis/projects/woof_compare_reports/nogit/pcgr/out/CUP-Pairs8__PRJ180660/CUP-Pairs8__PRJ180660.pcgr.snvs_indels.tiers.tsv",                          "/Users/pdiakumis/projects/woof_compare_reports/nogit/umccrise_2.1.2_rc6/umccrised/CUP-Pairs8__PRJ180660/small_variants/CUP-Pairs8__PRJ180660-somatic.pcgr.snvs_indels.tiers.tsv",
)

for (i in seq_len(nrow(x))) {
  print(x$sample[i])
  print(pcgr_cmp(x$run1[i], x$run2[i]))
  print("-----")
}
