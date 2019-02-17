# Sandbox for trying out things
require(tidyverse)
require(yaml)

config <- yaml::read_yaml("data/2019-02-15T0112_Patients_WGS_SFRC01116-merged.yaml")
s1 <- config$details[[1]]
lapply(config$details, bcbio_sample)
bcbio_sample <- function(x) {
  structure(
    list(
      sample_name = x$description,
      batch = x$metadata$batch,
      phenotype = x$metadata$phenotype),
    class = "bcbio_sample")
}
