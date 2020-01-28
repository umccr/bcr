# Sandbox for experimentation
# require(tidyverse)
# require(woofr)
require(here)

# Copy Number Variants
cnv1 <- here("nogit/data/umccrise_0.15.12/p33/umccrised/p33/2016_249_17_MH_P033__CCR170115b_MH17T002P033.purple.gene.cnv")
cnv2 <- here("nogit/data/umccrise_0.15.6/p33/umccrised/p33/2016_249_17_MH_P033__CCR170115b_MH17T002P033.purple.gene.cnv")
compare_purple_gene_files(cnv1, cnv2, out_cn_diff = here("nogit/cnv/cn_diff.tsv"), here("nogit/cnv/coord_diff.tsv"))

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
samplename <- sample
outdir <- "nogit/circos"
d1 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/final")
d2 <- glue::glue("~/Desktop/projects/umccr/woof/nogit/data/umccrise_0.15.12/{sample}/umccrised/{sample}")

manta_fnames <- list.files(c(d1, d2), pattern = "manta.vcf.gz$", recursive = TRUE, full.names = TRUE)
f1 <- manta_fnames[1]
f2 <- manta_fnames[2]

# read_manta_both(f1, f2)
mi <- manta_isec(f1, f2, samplename, "manta_bc_um", bnd_switch = TRUE)
mis <- manta_isec_stats(mi, sample, "manta_bc")
# mic <- get_circos(mi, sample, outdir)

# Gadi
sample <- "p33"
d1 <- "bcbio_116a0_GRCh38_native_umccrise_0.15.12/2016.249.17.MH.P033/final"
d2 <- "bcbio_116a0_GRCh38_native_umccrise_0.15.6/2016.249.17.MH.P033/final"
um1 <- "bcbio_116a0_GRCh38_native_umccrise_0.15.12/2016.249.17.MH.P033/umccrised/2016_249_17_MH_P033__CCR170115b_MH17T002P033"
um2 <- "bcbio_116a0_GRCh38_native_umccrise_0.15.6/2016.249.17.MH.P033/umccrised/2016_249_17_MH_P033__CCR170115b_MH17T002P033"
final1 <- bcbio_outputs(d1)
final2 <- bcbio_outputs(d2)
merge_bcbio_outputs(d1, d2, sample)
