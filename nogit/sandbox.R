# Sandbox for experimentation
# require(tidyverse)
# require(woofr)

sample <- "p33"
d1 <- glue::glue("~/Desktop/projects/umccr/woof_compare/test/data/umccrise_0.15.12/{sample}/final")
d2 <- glue::glue("~/Desktop/projects/umccr/woof_compare/test/data/umccrise_0.15.6/{sample}/final")
final1 <- bcbio_outputs(d1)
final2 <- bcbio_outputs(d2)
merge_bcbio_outputs(d1, d2, sample)



um1 <- glue::glue("~/Desktop/projects/umccr/woof_compare/test/data/umccrise_0.15.12/{sample}/umccrised/{sample}")
um2 <- glue::glue("~/Desktop/projects/umccr/woof_compare/test/data/umccrise_0.15.6/{sample}/umccrised/{sample}")
umccrise_outputs(um1)
umccrise_outputs(um2)
merge_umccrise_outputs(um1, um2, sample)
