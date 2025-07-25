setClass("ParamSpectraTools", representation = "VIRTUAL")

setClassUnion("nullOrCharacter", c("NULL", "character"))
setClassUnion("nullOrNumeric", c("NULL", "numeric"))
setClassUnion("subsetting", c("logical", "integer", "character"))


# ParseSpectraParam class: spectra parsing parameters
#' @export
setClass("ParseSpectraParam",
         slots = c(type = "character",
                   denoise = "logical",
                   ms2range = "nullOrNumeric",
                   mzIgnored = "nullOrNumeric",
                   includePrecursor = "logical",
                   ppmPrecursorFilter = "numeric",
                   normIntensity = "logical",
                   intensityNormedTo = "numeric",
                   thrIntensityRel = "numeric",
                   thrIntensityAbs = "numeric",
                   intensityNormed = "logical",
                   colSpectra = "numeric",
                   nameSpectra = "character",
                   skip = "numeric",
                   labelKeep = "nullOrCharacter",
                   labelName = "nullOrCharacter",
                   labelReparse = "nullOrCharacter",
                   sepReparse = "nullOrCharacter",
                   labelMerge = "nullOrCharacter",
                   sepMerge = "nullOrCharacter",
                   resDefineAt = "numeric"
         ),
         contains = c("ParamSpectraTools")
)

# SearchParam class: spectra searching parameters
#' @export
setClass("SearchParam",
         slots = c(ppm = "numeric",
                   scoreRT = "logical",
                   toleranceRT = "numeric",
                   scoreCCS = "logical",
                   toleranceCCS = "numeric",
                   typeCCS = "character",
                   adductIncluded = "nullOrCharacter",
                   adductExcluded = "nullOrCharacter",
                   adductFile = "nullOrCharacter",
                   classIncluded = "nullOrCharacter",
                   classExcluded = "nullOrCharacter",
                   useMS1ResDefine = 'logical',
                   updateRefMZ = 'logical',
                   resDefineAt = "numeric"
         ),
         contains = c("ParamSpectraTools")
)


# MatchParam class: spectra matching parameters
#' @export
setClass("MatchParam",
         slots = c(ppm = "numeric",
                   cutoff = "numeric",
                   methodMatch = "character",
                   methodScore = "character",
                   weightMZ = "numeric",
                   weightIntensity = "numeric",
                   includePrecursor = "logical",
                   ppmPrecursorFilter = "numeric",
                   ms2range = "nullOrNumeric",
                   thrIntensityAbs = "nullOrNumeric",
                   thrIntensityRel = "nullOrNumeric",
                   intensityExpNormed = "logical",
                   intensityLibNormed = "logical",
                   tuneLibSpectra = "logical",
                   useMS1ResDefine = 'logical',
                   resDefineAt = "numeric",
                   normIntensity = 'logical',
                   intensityNormedMethod = 'character'
         ),
         contains = c("ParamSpectraTools")
)


# MatchParam class: spectra matching parameters
#' @export
setClass("CombineParam",
         slots = c(cutoff = "numeric",
                   weightRT = "numeric",
                   weightCCS = "numeric",
                   weightMSMS = "numeric",
                   scoreMSMS = "character"
         ),
         contains = c("ParamSpectraTools")
)


#' @export
setClass("SpectraData",
         slots = c(
           info = "data.frame",
           spectra = "list"
         ))

#' @export
setClass("MatchScore",
         slots = c(
           info = "data.frame",
           matchedFragments = "list",
           nlFragments = "list"
         ))

#' @export
setMethod("show", signature(object = "SpectraData"), function(object) {
  cat("SpectraData object of", nrow(object@info),"precursors, ")
  nspec <- sum(!sapply(object@spectra, is.null))
  cat(nspec, "of which have MSMS spectra")
})

#' @export
setMethod("show", signature(object = "MatchScore"), function(object) {
  cat("MatchScore object of", nrow(object@info),"reference fragments!")
})
