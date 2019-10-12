# Sandbox for experimentation
require(tidyverse)
require(yaml)
require(woofr)

d1 <- "~/Desktop/projects/umccr/woof_compare/test/data/f1"
d2 <- "~/Desktop/projects/umccr/woof_compare/test/data/f2"
sample <- "foo"
bcbio_outputs(d1)
bcbio_outputs(d2)
merge_bcbio_outputs(d1, d2, "foo")
