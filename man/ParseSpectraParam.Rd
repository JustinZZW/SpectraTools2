% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Function-Params.R, R/Method-Params.R
\name{ParseSpectraParam}
\alias{ParseSpectraParam}
\alias{type,ParseSpectraParam-method}
\alias{type}
\alias{type<-,ParseSpectraParam-method}
\alias{type<-}
\alias{denoise,ParseSpectraParam-method}
\alias{denoise}
\alias{denoise<-,ParseSpectraParam-method}
\alias{denoise<-}
\alias{ms2range,ParseSpectraParam-method}
\alias{ms2range}
\alias{ms2range<-,ParseSpectraParam-method}
\alias{ms2range<-}
\alias{mzIgnored,ParseSpectraParam-method}
\alias{mzIgnored}
\alias{mzIgnored<-,ParseSpectraParam-method}
\alias{mzIgnored<-}
\alias{includePrecursor,ParseSpectraParam-method}
\alias{includePrecursor}
\alias{includePrecursor<-,ParseSpectraParam-method}
\alias{includePrecursor<-}
\alias{ppmPrecursorFilter,ParseSpectraParam-method}
\alias{ppmPrecursorFilter}
\alias{ppmPrecursorFilter<-,ParseSpectraParam-method}
\alias{ppmPrecursorFilter<-}
\alias{normIntensity,ParseSpectraParam-method}
\alias{normIntensity}
\alias{normIntensity<-,ParseSpectraParam-method}
\alias{normIntensity<-}
\alias{intensityNormedTo,ParseSpectraParam-method}
\alias{intensityNormedTo}
\alias{intensityNormedTo<-,ParseSpectraParam-method}
\alias{intensityNormedTo<-}
\alias{thrIntensityRel,ParseSpectraParam-method}
\alias{thrIntensityRel}
\alias{thrIntensityRel<-,ParseSpectraParam-method}
\alias{thrIntensityRel<-}
\alias{thrIntensityAbs,ParseSpectraParam-method}
\alias{thrIntensityAbs}
\alias{thrIntensityAbs<-,ParseSpectraParam-method}
\alias{thrIntensityAbs<-}
\alias{intensityNormed,ParseSpectraParam-method}
\alias{intensityNormed}
\alias{intensityNormed<-,ParseSpectraParam-method}
\alias{intensityNormed<-}
\alias{colSpectra,ParseSpectraParam-method}
\alias{colSpectra}
\alias{colSpectra<-,ParseSpectraParam-method}
\alias{colSpectra<-}
\alias{nameSpectra,ParseSpectraParam-method}
\alias{nameSpectra}
\alias{nameSpectra<-,ParseSpectraParam-method}
\alias{nameSpectra<-}
\alias{skip,ParseSpectraParam-method}
\alias{skip}
\alias{skip<-,ParseSpectraParam-method}
\alias{skip<-}
\alias{labelKeep,ParseSpectraParam-method}
\alias{labelKeep}
\alias{labelKeep<-,ParseSpectraParam-method}
\alias{labelKeep<-}
\alias{labelName,ParseSpectraParam-method}
\alias{labelName}
\alias{labelName<-,ParseSpectraParam-method}
\alias{labelName<-}
\alias{labelReparse,ParseSpectraParam-method}
\alias{labelReparse}
\alias{labelReparse<-,ParseSpectraParam-method}
\alias{labelReparse<-}
\alias{sepReparse,ParseSpectraParam-method}
\alias{sepReparse}
\alias{sepReparse<-,ParseSpectraParam-method}
\alias{sepReparse<-}
\alias{labelMerge,ParseSpectraParam-method}
\alias{labelMerge}
\alias{labelMerge<-,ParseSpectraParam-method}
\alias{labelMerge<-}
\alias{sepMerge,ParseSpectraParam-method}
\alias{sepMerge}
\alias{sepMerge<-,ParseSpectraParam-method}
\alias{sepMerge<-}
\title{Parsing Spectra setup}
\usage{
ParseSpectraParam(
  type = c("mgf", "msp", "cef", "raw", "csv", "txt"),
  denoise = TRUE,
  ms2range = NULL,
  mzIgnored = NULL,
  ppmPrecursorFilter = 10,
  includePrecursor = TRUE,
  normIntensity = FALSE,
  intensityNormedTo = 1,
  thrIntensityRel = 0,
  thrIntensityAbs = 100,
  intensityNormed = FALSE,
  colSpectra = c(1, 2),
  nameSpectra = c("mz", "intensity"),
  skip = 0,
  labelKeep = NULL,
  labelName = NULL,
  labelReparse = NULL,
  sepReparse = NULL,
  labelMerge = NULL,
  sepMerge = NULL,
  resDefineAt = 400
)

\S4method{type}{ParseSpectraParam}(object)

\S4method{type}{ParseSpectraParam}(object) <- value

\S4method{denoise}{ParseSpectraParam}(object)

\S4method{denoise}{ParseSpectraParam}(object) <- value

\S4method{ms2range}{ParseSpectraParam}(object)

\S4method{ms2range}{ParseSpectraParam}(object) <- value

\S4method{mzIgnored}{ParseSpectraParam}(object)

\S4method{mzIgnored}{ParseSpectraParam}(object) <- value

\S4method{includePrecursor}{ParseSpectraParam}(object)

\S4method{includePrecursor}{ParseSpectraParam}(object) <- value

\S4method{ppmPrecursorFilter}{ParseSpectraParam}(object)

\S4method{ppmPrecursorFilter}{ParseSpectraParam}(object) <- value

\S4method{normIntensity}{ParseSpectraParam}(object)

\S4method{normIntensity}{ParseSpectraParam}(object) <- value

\S4method{intensityNormedTo}{ParseSpectraParam}(object)

\S4method{intensityNormedTo}{ParseSpectraParam}(object) <- value

\S4method{thrIntensityRel}{ParseSpectraParam}(object)

\S4method{thrIntensityRel}{ParseSpectraParam}(object) <- value

\S4method{thrIntensityAbs}{ParseSpectraParam}(object)

\S4method{thrIntensityAbs}{ParseSpectraParam}(object) <- value

\S4method{intensityNormed}{ParseSpectraParam}(object)

\S4method{intensityNormed}{ParseSpectraParam}(object) <- value

\S4method{colSpectra}{ParseSpectraParam}(object)

\S4method{colSpectra}{ParseSpectraParam}(object) <- value

\S4method{nameSpectra}{ParseSpectraParam}(object)

\S4method{nameSpectra}{ParseSpectraParam}(object) <- value

\S4method{skip}{ParseSpectraParam}(object)

\S4method{skip}{ParseSpectraParam}(object) <- value

\S4method{labelKeep}{ParseSpectraParam}(object)

\S4method{labelKeep}{ParseSpectraParam}(object) <- value

\S4method{labelName}{ParseSpectraParam}(object)

\S4method{labelName}{ParseSpectraParam}(object) <- value

\S4method{labelReparse}{ParseSpectraParam}(object)

\S4method{labelReparse}{ParseSpectraParam}(object) <- value

\S4method{sepReparse}{ParseSpectraParam}(object)

\S4method{sepReparse}{ParseSpectraParam}(object) <- value

\S4method{labelMerge}{ParseSpectraParam}(object)

\S4method{labelMerge}{ParseSpectraParam}(object) <- value

\S4method{sepMerge}{ParseSpectraParam}(object)

\S4method{sepMerge}{ParseSpectraParam}(object) <- value
}
\arguments{
\item{type}{\code{character} spectral file types ("MSP", "MGF" and "CSV" format are
supported)}

\item{denoise}{\code{logical} if denoising is needed}

\item{ms2range}{\code{numeric} fragment mz range}

\item{ppmPrecursorFilter}{\code{logical} ppm tolerance to determine precursor
fragment}

\item{includePrecursor}{\code{logical} if include precursor fragment}

\item{normIntensity}{\code{logical} if normalize intensities}

\item{intensityNormedTo}{\code{numeric} intensity to be normalized to}

\item{thrIntensityRel}{\code{numeric} relative intensity threshold to be removed}

\item{thrIntensityAbs}{\code{numeric} absolute intensity threshold to be removed}

\item{intensityNormed}{\code{logical} if the spectral intensity is normalized
in spectrum files}

\item{colSpectra}{\code{integer vector} columns for recording mz and intensities
(the first two items must be in the order of "mz" and "intensity")}

\item{nameSpectra}{\code{character vector} colnames of spectra (the first two items
must be "mz" and "intensity")}

\item{skip}{\code{integer} the number of line of the data file to skip before
beginning to read data for "CSV" spectra.}

\item{labelKeep}{\code{character vector} labels to be kept in precursor information
in the generated spectra. Labels for "name" and "labid" must be included for
generating library spectra}

\item{labelName}{\code{character vector} names of the labels to be kept in precursor
information in the generated spectra. "name" and "labid" must be included for
generating library spectra}

\item{labelReparse}{\code{character vector} names of the labels to be re-parsed in
precursor information in the generated spectra.}

\item{sepReparse}{\code{character} which character to be seperated by for re-parsing}

\item{labelMerge}{\code{character vector} names of the labels to be merged in
precursor information in the generated spectra.}

\item{sepMerge}{\code{character} which character to be seperated by for merging}

\item{resDefineAt}{\code{numeric} m/z (Da) value for resolution definition}

\item{value}{The value for the slot.}

\item{mzIgnoreded}{\code{numeric} mz of fragment to be ignored when normalizing or
denoising with relative intensity threshold (only one fragment is supported)}
}
\value{
an \code{ParseSpectraParam} object
}
\description{
\code{type},\code{type<-}: getter and setter for the \code{type}
    slot of the object.

\code{denoise},\code{denoise<-}: getter and setter for the \code{denoise}
    slot of the object.

\code{ms2range},\code{ms2range<-}: getter and setter for the \code{ms2range}
    slot of the object.

\code{mzIgnored},\code{mzIgnored<-}: getter and setter for the \code{mzIgnored}
    slot of the object.

\code{includePrecursor},\code{includePrecursor<-}: getter and setter for the \code{includePrecursor}
    slot of the object.

\code{ppmPrecursorFilter},\code{ppmPrecursorFilter<-}: getter and setter for the \code{ppmPrecursorFilter}
    slot of the object.

\code{normIntensity},\code{normIntensity<-}: getter and setter for the \code{normIntensity}
    slot of the object.

\code{intensityNormedTo},\code{intensityNormedTo<-}: getter and setter for the \code{intensityNormedTo}
    slot of the object.

\code{thrIntensityRel},\code{thrIntensityRel<-}: getter and setter for the \code{thrIntensityRel}
    slot of the object.

\code{thrIntensityAbs},\code{thrIntensityAbs<-}: getter and setter for the \code{thrIntensityAbs}
    slot of the object.

\code{intensityNormed},\code{intensityNormed<-}: getter and setter for the \code{intensityNormed}
    slot of the object.

\code{colSpectra},\code{colSpectra<-}: getter and setter for the \code{colSpectra}
    slot of the object.

\code{nameSpectra},\code{nameSpectra<-}: getter and setter for the \code{nameSpectra}
    slot of the object.

\code{skip},\code{skip<-}: getter and setter for the \code{skip}
    slot of the object.

\code{labelKeep},\code{labelKeep<-}: getter and setter for the \code{labelKeep}
    slot of the object.

\code{labelName},\code{labelName<-}: getter and setter for the \code{labelName}
    slot of the object.

\code{labelReparse},\code{labelReparse<-}: getter and setter for the \code{labelReparse}
    slot of the object.

\code{sepReparse},\code{sepReparse<-}: getter and setter for the \code{sepReparse}
    slot of the object.

\code{labelMerge},\code{labelMerge<-}: getter and setter for the \code{labelMerge}
    slot of the object.

\code{sepMerge},\code{sepMerge<-}: getter and setter for the \code{sepMerge}
    slot of the object.
}
