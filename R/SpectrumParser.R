
#' @title Spectrum parser
#'
#' @name SpectrumParser
#'
#' @description
#' 
#' Classes extending the base `SpectrumParser` object are supposed to read
#' spectrum data from a certain input format and return a standardized output
#' format.
#'
#' @md
#'
#' @author Johannes Rainer
#'
#' @exportClass SpectrumParser
NULL

setClass("SpectrumParser",
         slots = c(schema = "list"),
         prototype = prototype(schema = list()),
         contains = "VIRTUAL")

#' @description
#'
#' `importSpectrum` reads spectrum data from a file, extracted all fields and
#' maps them to the corresponding *standard* fields using the parser's schema
#' definition.
#'
#' @param file `character(1)` with the name of the file from which spectrum
#'     data should be imported.
#'
#' @param object object extending `SpectrumParser`.
#'
#' @return a `Spectra` object - TODO needs to be discussed!
#' 
#' @md
#'
#' @exportMethod importSpectrum
#' 
#' @rdname SpectrumParser
setGeneric("importSpectrum", function(object, file, ...)
    standardGeneric("importSpectrum"))
setMethod("importSpectrum", "SpectrumParser", function(object, file, ...)
          stop("importSpectrum not implemented for ", class(object)))

setMethod("show", "SpectrumParser", function(object) {
    cat("Object of class", class(object), "\n")
})

