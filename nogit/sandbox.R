# Sandbox for experimentation
# require(tidyverse)
# require(woofr)

sample <- "p33"
d1 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/final")
d2 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.6/{sample}/final")
final1 <- bcbio_outputs(d1)
final2 <- bcbio_outputs(d2)
merge_bcbio_outputs(d1, d2, sample)

um1 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/umccrised/{sample}")
um2 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.6/{sample}/umccrised/{sample}")
umccrise_outputs(um1)
umccrise_outputs(um2)
merge_umccrise_outputs(um1, um2, sample)


# Structural Variants
sample <- "p25"
d1 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/final")
d2 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/umccrised/{sample}")

manta_fnames <- list.files(c(d1, d2), pattern = "manta.vcf.gz$", recursive = TRUE, full.names = TRUE)
f1 <- manta_fnames[1]
f2 <- manta_fnames[2]

# read_manta_both(f1, f2)
mi <- manta_isec(f1, f2, bnd_switch = TRUE)
manta_isec_stats(mi, sample, "manta_bc")
get_circos(mi, sample, "nogit/circos", "manta")
