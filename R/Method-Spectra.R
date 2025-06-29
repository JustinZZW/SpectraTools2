#' Export spectra
#' Export spectra to a text file
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @param exportFile \code{character} File path to be exported
#' @param fileType \code{character} Exported file type (\code{msp} or \code{mgf})
#' @param infoColumns \code{character} Columns to be exported in spectra info
#' @rdname Method-Spectra
#' @export
setMethod(
  "ExportSpectra",
  signature = c("SpectraData", "character", "character", "ANY"),
  function(spectra, exportFile, fileType = c("msp", "mgf"), infoColumns = NULL) {
    fileType <- match.arg(fileType)

    if (missing(infoColumns)) {
    } else if (all(infoColumns %in% colnames(spectra@info))) {
      spectra@info <- spectra@info[, infoColumns, drop = FALSE]
    } else {
      stop("Invalid infoColumns. Please confirm all your infoColumns are in the spectra data.")
    }

    switch(tolower(fileType),
           "msp" = .ExportMSP(spectra, exportFile),
           "mgf" = .ExportMGF(spectra, exportFile)
    )
  }
)

#' Find spectra
#' Find spectra with user defined terms
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @param findby \code{character} Terms for spectra finding
#' @param value \code{character} Values of terms for spectra finding
#' @return a \code{SpectraData} object
#' @rdname Method-Spectra
#' @export
setMethod(
  "FindSpectra",
  signature = c("SpectraData"),
  function(spectra, findby = NULL, values = NULL, colSpecName = NULL){
    isKeep <- apply(spectra@info[, findby, drop = FALSE], 1,
                    function(dr) {
                      all(dr == as.character(values))
                    })
    idxKeep <- which(isKeep)
    if (length(idxKeep) == 0) {
      warning("No spectra found and returning NULL as result!")
      return(NULL)
    } else {
      return(SpectraData(spectra = spectra@spectra[idxKeep],
                         info = spectra@info[idxKeep, , drop = FALSE],
                         colSpecName = colSpecName))
    }
  }
)

#' Filter fragments
#' Filtering fragments in SpectraData with mass range and fragment intensity
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @param mzrange \code{numeric(2)} Fragment mass range
#' @param thrIntensityAbs \code{numeric} Absolute fragment intensity threshold to be kept
#' @param thrIntensityRel \code{numeric} Relative fragment intensity threshold to be kept
#' @return a \code{SpectraData} object
#' @rdname Method-Spectra
#' @export
setMethod(
  "FilterFragments",
  signature = c("SpectraData"),
  function(spectra, mzrange = NULL, thrIntensityAbs = NULL, thrIntensityRel = NULL){
    spectra@spectra <- lapply(spectra@spectra, function(spec) {
      isKeep <- rep(TRUE, nrow(spec))
      if (!is.null(mzrange)) {
        isKeep <- isKeep & spec[, "mz"] >= mzrange[1] & spec[, "mz"] <= mzrange[2]
      }
      if (!is.null(thrIntensityAbs)) {
        isKeep <- isKeep & spec[, "intensity"] >= thrIntensityAbs
      }
      if (!is.null(thrIntensityRel)) {
        isKeep <- isKeep & spec[, "intensity"] >= thrIntensityRel
      }
      spec[isKeep, , drop = FALSE]
    })
    return(spectra)
  }
)

#' Filter NULL spectra
#' Filter the NULL spectra from SpectraData
#' @param spectra \code{SpectraData} Spectra object for the spectra to be found
#' @return a \code{SpectraData} object
#' @rdname Method-Spectra
#' @export
setMethod(
  "FilterNULL",
  signature = c("SpectraData"),
  function(spectra, keepInfo = TRUE){
    isKeep <- !sapply(spectra@spectra, is.null)
    idxKeep <- which(isKeep)
    if (length(idxKeep) == 0) {
      warning("No spectra found and returning NULL as result!")
      return(NULL)
    } else {
      if (keepInfo) {
        info <- spectra@info
      } else {
        info <- spectra@info[idxKeep, , drop = FALSE]
      }

      return(SpectraData(spectra = spectra@spectra[idxKeep],
                         info = info))
    }
  }
)


#' @export
setMethod(
  "DenoiseSpectra",
  signature = c("SpectraData"),
  function(spectra,
           resetIdx = FALSE,
           mzIgnored = NULL,
           mzPrecursor = NULL,
           ppmPrecursorFilter = 20,
           ms2range = NULL,
           includePrecursor = TRUE,
           normIntensity = FALSE,
           intensityNormedTo = 1,
           intensityNormed = FALSE,
           detectIntensityAbs = FALSE,
           thrDetectCounts = 5,
           thrIntensityAbs = NULL,
           thrIntensityRel = 0.0,
           snthresh = 3,
           ms2noise = 3,
           checkSanity = TRUE,
           thrIntensitySanity = 50,
           useMS1ResDefine = TRUE,
           resDefineAt = 400,
           intensityNormedMethod = c('maximum', 'bonanza', 'gnps')) {
    intensityNormedMethod <- match.arg(intensityNormedMethod)
    if (is.null(mzPrecursor) & "mz" %in% colnames(spectra@info)) {
      mzPrecursor <- spectra@info[, "mz"]
    }
    spec <- lapply(seq_along(spectra@spectra), function(idx) {
      DeNoise(spectra@spectra[[idx]],
              mzIgnored = mzIgnored,
              mzPrecursor = mzPrecursor[idx],
              ppmPrecursorFilter = ppmPrecursorFilter,
              ms2range = ms2range,
              includePrecursor = includePrecursor,
              normIntensity = normIntensity,
              intensityNormedTo = intensityNormedTo,
              intensityNormed = intensityNormed,
              detectIntensityAbs = detectIntensityAbs,
              thrDetectCounts = thrDetectCounts,
              thrIntensityAbs = thrIntensityAbs,
              thrIntensityRel = thrIntensityRel,
              snthresh = snthresh,
              ms2noise = ms2noise,
              checkSanity = checkSanity,
              thrIntensitySanity = thrIntensitySanity,
              useMS1ResDefine = useMS1ResDefine,
              resDefineAt = resDefineAt,
              intensityNormedMethod = intensityNormedMethod)

    })
    isKeep <- !sapply(spec, is.null)
    spec <- spec[isKeep]
    specInfo <- spectra@info[isKeep, , drop = FALSE]
    if (resetIdx) {
      specidx <- paste0("#", seq(nrow(specInfo)))
      names(spec) <- rownames(specInfo) <- specidx
    }

    return(SpectraData(info = specInfo, spectra = spec))
  }
)

#' @export
setMethod("[",
          signature = c("SpectraData", "subsetting", "missing", "ANY"),
          function(x, i, j, ..., drop=TRUE)
          {
            spectra <-  x@spectra[i]
            info <- x@info[i, , drop = FALSE]
            if (drop) {
              spectra <- spectra[!sapply(spectra, is.null)]
              info <- info[names(spectra), , drop = FALSE]
            }
            initialize(x, info = info, spectra = spectra)
          })


#' @export
setMethod("[[",
          signature = c("SpectraData", "subsetting", "missing"),
          function(x, i, j, ...)
          {
            x@spectra[[i]]
          })

#' @export
setMethod("names",
          signature = c("SpectraData"),
          function(x)
          {
            names(x@spectra)
          })

#' @export
setMethod(
  "UpdateNames",
  signature = c("SpectraData"),
  function(x, names) {
    if (!is.vector(names)) {
      stop("names must be a vector")
    }
    if (length(names) != length(x)) {
      stop("Length of names does not match length of spectra!")
    }
    row_match <- match(rownames(x@info), names(x@spectra))
    if (any(rownames(x@info[-row_match, , drop=FALSE]) %in% names)) {
      stop("Some names are not unique!")
    }
    names(x@spectra) <- names
    rownames(x@info)[row_match] <- names
    if (validObject(x))
      return(x)
  })


#' @export
setMethod("length",
          signature = c("SpectraData"),
          function(x)
          {
            length(x@spectra)
          })
