setGeneric("UpdateNames", function(x, names, ...) standardGeneric("UpdateNames"))

setGeneric("ParseSpectra",  function(parseParam,...)
  standardGeneric("ParseSpectra"))

setGeneric("SearchSpectra", function(dataExp, dataRef, searchParam, ...)
  standardGeneric("SearchSpectra"))

setGeneric("MatchSpectra", function(dataExp, dataRef, matchParam, ...)
  standardGeneric("MatchSpectra"))

setGeneric("GetMatchScore.direct", function(dataExp, dataRef, matchParam, ...)
  standardGeneric("GetMatchScore.direct"))

setGeneric("GetMatchScore.bootstrap", function(dataExp, dataRef, matchParam, ...)
  standardGeneric("GetMatchScore.bootstrap"))

setGeneric("CombineScore", function(matchScore, combineParam, ...)
  standardGeneric("CombineScore"))

setGeneric("GenOutputScore", function(matchScore, ...)
  standardGeneric("GenOutputScore"))

setGeneric("ApplyRules", function(matchScore, ...)
  standardGeneric("ApplyRules"))

setGeneric("RefineRuleResult", function(matchScore, ...)
  standardGeneric("RefineRuleResult"))

setGeneric("PlotMirror", function(matchScore, ...)
  standardGeneric("PlotMirror"))

setGeneric("ExportSpectra", function(spectra, exportFile, fileType, infoColumns, ...)
  standardGeneric("ExportSpectra"))
setGeneric("FindSpectra", function(spectra, ...) standardGeneric("FindSpectra"))
setGeneric("FilterFragments", function(spectra, ...) standardGeneric("FilterFragments"))
setGeneric("FilterNULL", function(spectra, ...) standardGeneric("FilterNULL"))
setGeneric("DenoiseSpectra", function(spectra, ...) standardGeneric("DenoiseSpectra"))

# setGeneric("AsList", function(x, ...) standardGeneric("AsList"))

#################################
## ParseSpectraParam
#################################
setGeneric("type", function(object, ...) standardGeneric("type"))
setGeneric("type<-", function(object, ...) standardGeneric("type<-"))
setGeneric("denoise", function(object, ...) standardGeneric("denoise"))
setGeneric("denoise<-", function(object, ...) standardGeneric("denoise<-"))
setGeneric("ms2range", function(object, ...) standardGeneric("ms2range"))
setGeneric("ms2range<-", function(object, ...) standardGeneric("ms2range<-"))
setGeneric("mzIgnored", function(object, ...) standardGeneric("mzIgnored"))
setGeneric("mzIgnored<-", function(object, ...) standardGeneric("mzIgnored<-"))
setGeneric("ppmPrecursorFilter", function(object, ...) standardGeneric("ppmPrecursorFilter"))
setGeneric("ppmPrecursorFilter<-", function(object, ...) standardGeneric("ppmPrecursorFilter<-"))
setGeneric("includePrecursor", function(object, ...) standardGeneric("includePrecursor"))
setGeneric("includePrecursor<-", function(object, ...) standardGeneric("includePrecursor<-"))
setGeneric("normIntensity", function(object, ...) standardGeneric("normIntensity"))
setGeneric("normIntensity<-", function(object, ...) standardGeneric("normIntensity<-"))
setGeneric("intensityNormedTo", function(object, ...) standardGeneric("intensityNormedTo"))
setGeneric("intensityNormedTo<-", function(object, ...) standardGeneric("intensityNormedTo<-"))
setGeneric("thrIntensityRel", function(object, ...) standardGeneric("thrIntensityRel"))
setGeneric("thrIntensityRel<-", function(object, ...) standardGeneric("thrIntensityRel<-"))
setGeneric("thrIntensityAbs", function(object, ...) standardGeneric("thrIntensityAbs"))
setGeneric("thrIntensityAbs<-", function(object, ...) standardGeneric("thrIntensityAbs<-"))
setGeneric("intensityNormed", function(object, ...) standardGeneric("intensityNormed"))
setGeneric("intensityNormed<-", function(object, ...) standardGeneric("intensityNormed<-"))
setGeneric("colSpectra", function(object, ...) standardGeneric("colSpectra"))
setGeneric("colSpectra<-", function(object, ...) standardGeneric("colSpectra<-"))
setGeneric("nameSpectra", function(object, ...) standardGeneric("nameSpectra"))
setGeneric("nameSpectra<-", function(object, ...) standardGeneric("nameSpectra<-"))
setGeneric("skip", function(object, ...) standardGeneric("skip"))
setGeneric("skip<-", function(object, ...) standardGeneric("skip<-"))
setGeneric("labelKeep", function(object, ...) standardGeneric("labelKeep"))
setGeneric("labelKeep<-", function(object, ...) standardGeneric("labelKeep<-"))
setGeneric("labelName", function(object, ...) standardGeneric("labelName"))
setGeneric("labelName<-", function(object, ...) standardGeneric("labelName<-"))
setGeneric("labelReparse", function(object, ...) standardGeneric("labelReparse"))
setGeneric("labelReparse<-", function(object, ...) standardGeneric("labelReparse<-"))
setGeneric("sepReparse", function(object, ...) standardGeneric("sepReparse"))
setGeneric("sepReparse<-", function(object, ...) standardGeneric("sepReparse<-"))
setGeneric("labelMerge", function(object, ...) standardGeneric("labelMerge"))
setGeneric("labelMerge<-", function(object, ...) standardGeneric("labelMerge<-"))
setGeneric("sepMerge", function(object, ...) standardGeneric("sepMerge"))
setGeneric("sepMerge<-", function(object, ...) standardGeneric("sepMerge<-"))

#################################
## SearchSpectraParam
#################################
setGeneric("ppm", function(object, ...) standardGeneric("ppm"))
setGeneric("ppm<-", function(object, ...) standardGeneric("ppm<-"))
setGeneric("scoreRT", function(object, ...) standardGeneric("scoreRT"))
setGeneric("scoreRT<-", function(object, ...) standardGeneric("scoreRT<-"))
setGeneric("toleranceRT", function(object, ...) standardGeneric("toleranceRT"))
setGeneric("toleranceRT<-", function(object, ...) standardGeneric("toleranceRT<-"))
setGeneric("scoreCCS", function(object, ...) standardGeneric("scoreCCS"))
setGeneric("scoreCCS<-", function(object, ...) standardGeneric("scoreCCS<-"))
setGeneric("toleranceCCS", function(object, ...) standardGeneric("toleranceCCS"))
setGeneric("toleranceCCS<-", function(object, ...) standardGeneric("toleranceCCS<-"))
setGeneric("typeCCS", function(object, ...) standardGeneric("typeCCS"))
setGeneric("typeCCS<-", function(object, ...) standardGeneric("typeCCS<-"))
setGeneric("adductIncluded", function(object, ...) standardGeneric("adductIncluded"))
setGeneric("adductIncluded<-", function(object, ...) standardGeneric("adductIncluded<-"))
setGeneric("adductExcluded", function(object, ...) standardGeneric("adductExcluded"))
setGeneric("adductExcluded<-", function(object, ...) standardGeneric("adductExcluded<-"))
setGeneric("adductFile", function(object, ...) standardGeneric("adductFile"))
setGeneric("adductFile<-", function(object, ...) standardGeneric("adductFile<-"))
setGeneric("classIncluded", function(object, ...) standardGeneric("classIncluded"))
setGeneric("classIncluded<-", function(object, ...) standardGeneric("classIncluded<-"))
setGeneric("classExcluded", function(object, ...) standardGeneric("classExcluded"))
setGeneric("classExcluded<-", function(object, ...) standardGeneric("classExcluded<-"))
setGeneric("resDefineAt", function(object, ...) standardGeneric("resDefineAt"))
setGeneric("resDefineAt<-", function(object, ...) standardGeneric("resDefineAt<-"))

#################################
## MatchSpectraParam
#################################
setGeneric("ppm", function(object, ...) standardGeneric("ppm"))
setGeneric("ppm<-", function(object, ...) standardGeneric("ppm<-"))
setGeneric("cutoff", function(object, ...) standardGeneric("cutoff"))
setGeneric("cutoff<-", function(object, ...) standardGeneric("cutoff<-"))
setGeneric("methodMatch", function(object, ...) standardGeneric("methodMatch"))
setGeneric("methodMatch<-", function(object, ...) standardGeneric("methodMatch<-"))
setGeneric("methodScore", function(object, ...) standardGeneric("methodScore"))
setGeneric("methodScore<-", function(object, ...) standardGeneric("methodScore<-"))
setGeneric("weightMZ", function(object, ...) standardGeneric("weightMZ"))
setGeneric("weightMZ<-", function(object, ...) standardGeneric("weightMZ<-"))
setGeneric("weightIntensity", function(object, ...) standardGeneric("weightIntensity"))
setGeneric("weightIntensity<-", function(object, ...) standardGeneric("weightIntensity<-"))
setGeneric("intensityExpNormed", function(object, ...) standardGeneric("intensityExpNormed"))
setGeneric("intensityExpNormed<-", function(object, ...) standardGeneric("intensityExpNormed<-"))
setGeneric("intensityLibNormed", function(object, ...) standardGeneric("intensityLibNormed"))
setGeneric("intensityLibNormed<-", function(object, ...) standardGeneric("intensityLibNormed<-"))
setGeneric("tuneLibSpectra", function(object, ...) standardGeneric("tuneLibSpectra"))
setGeneric("tuneLibSpectra<-", function(object, ...) standardGeneric("tuneLibSpectra<-"))

#################################
## MatchSpectraParam
#################################
setGeneric("weightRT", function(object, ...) standardGeneric("weightRT"))
setGeneric("weightRT<-", function(object, ...) standardGeneric("weightRT<-"))
setGeneric("weightCCS", function(object, ...) standardGeneric("weightCCS"))
setGeneric("weightCCS<-", function(object, ...) standardGeneric("weightCCS<-"))
setGeneric("weightMSMS", function(object, ...) standardGeneric("weightMSMS"))
setGeneric("weightMSMS<-", function(object, ...) standardGeneric("weightMSMS<-"))
setGeneric("scoreMSMS", function(object, ...) standardGeneric("scoreMSMS"))
setGeneric("scoreMSMS<-", function(object, ...) standardGeneric("scoreMSMS<-"))
