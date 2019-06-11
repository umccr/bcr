require(methods)

x <- "data/2019-02-01T0241_Cromwell_WGS_CUP-Pairs8-merged.yaml"

setClass("BaseSample",
         slots = c(
           name = "character",
           phenotype = "character",
           gender = "character"),
         prototype = list(
           name = NA_character_,
           phenotype = NA_character_,
           gender = NA_character_
         ))

BaseSample <- function(name, phenotype = NA_character_, gender = NA_character_) {
  new("BaseSample", name = name, phenotype = phenotype, gender = gender)
}

setValidity(Class = "BaseSample", function(object) {
  if (!(object@phenotype %in% c(NA_character_, "tumor", "normal") && length(object@phenotype) == 1)) {
    "@phenotype should be 'tumor', 'normal', or left empty."
  } else if (!(object@gender %in% c(NA_character_, "male", "femal", "unknown") && length(object@gender) == 1)) {
    "@gender should be 'male', 'female', 'unknown', or left empty."
  } else {
    TRUE
  }
})

setClass("BcbioProject",
         slots = c(
           final_dir = "character",
           config_dir = "character",
           log_dir = "character",
           bcbio_yaml_fpath = "character",
           date_dir = "character"
         ),
         prototype = list(
           final_dir = NA_character_,
           config_dir = NA_character_,
           log_dir = NA_character_,
           bcbio_yaml_fpath = NA_character_,
           date_dir = NA_character_
         ))

setClass("BcbioBatch",
         slots = c(

         ),
         prototype = c(

         ))

setClass("BcbioSample",
         contains = "BaseSample",
         slots = c(
           bcbio_project = "BcbioProject",
           bam = "character",
           is_wgs = "logical",
           is_wts = "logical",
           genome = "character"),
         prototype = list(
           bcbio_project = new("BcbioProject"),
           bam = NA_character_,
           is_wgs = NA,
           is_wts = NA,
           genome = NA_character_))

# setter, getter
setGeneric("name", function(x) standardGeneric("name"))
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

# methods
setMethod("name", "BaseSample", function(x) x@name)
setMethod("name<-", "BaseSample", function(x, value) {
  x@name <- value
  x
})

john <- new("BaseSample", name = "John")
is(john, "BaseSample")
john@name
john@phenotype
slot(john, "phenotype")

name(john)
name(john) <- "Jane"

# read_bcbio_config <- function(x) {
#
#   algo <- function(key) {
#     sapply(conf$details, function(el) el$algorithm[[key]])
#   }
#   conf <- yaml::read_yaml(x)
#   res <- list()
#   res$final_dir <- conf$upload
#   res$aligners <- algo("aligner")
#   res$adapters <- algo("adapter")
#   res$svcallers <- algo("svcaller")
#   res$varcallers <- algo("variantcaller")
# }
