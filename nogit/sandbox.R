# Sandbox for experimentation
require(tidyverse)
require(yaml)
require(woofr)

x <- "nogit/data/2019-02-01T0241_Cromwell_WGS_CUP-Pairs8-merged.yaml"
b <- read_bcbio_configs(x, x)
vf <- b$variant_files
jsonlite::write_json(vf, "nogit/data/variant_files1.json", pretty = T, auto_unbox = T)
