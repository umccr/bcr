# Sandbox for trying out things
require(tidyverse)
require(yaml)
require(woofr)

final1 <- woofr::bcbio_outputs("~/my_apps/sshfs/raijin/native")
final2 <- woofr::bcbio_outputs("~/my_apps/sshfs/raijin/cwl")

final1 <- final1 %>%
  filter(!ftype %in% c("Manta", "OTHER"))

final2 <- final2 %>%
  filter(!ftype %in% c("Manta", "OTHER"))

stopifnot(all(final1$ftype == final2$ftype))

all <- left_join(final1, final2, by = "ftype")
readr::write_tsv(all, path = "bcbio_vcfs.tsv")
