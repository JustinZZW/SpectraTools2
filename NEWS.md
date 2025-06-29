# SpectraTools 0.2.0
* initial commit from YYD

# SpectraTools 0.2.1
* Added a `NEWS.md` file to track changes to the package.
* add hybrid, gnps and bonanza scoring approach
* add functions: `MatchNeutralLoss()`, `GetScore.hybrid()`, `GetScore.bonanza()`, `GetScore.gnps()`
* modified functions: add `intensityNormedMethod` parameter (`maximum`, `bonanza`, `gnps`)
* fix the bug: invalid parameter passing (`ppmPrecursorFilter`) leads the `NULL` value for one
  fragment match

# SpectraTools 0.2.2
* modify `minfracVote` to make sure at least two fragments are consistent when creating consensus
  MSMS spectra
* bugfix for `ConsensusSpectra` function
* bugfix for consensus parameter error
* bugfix for searching metabolites with adduct types by modifying reference m/z

# SpectraTools 0.2.3
* add adduct type `[M-2H]-` for negative mode

# SpectraTools 0.2.4
* bugfix for missing adduct types

# SpectraTools 0.2.5
* bugfix for mass difference of some adduct types
* add `useMS1ResDefine` parameter to `SearchParam` and `MatchParam`
* add `updateRefMZ` parameter to `SearchParam`

# SpectraTools 0.2.6
* bugfix: in case index of max int fragment is changed after fragment is removed

# SpectraTools 0.2.7
* add `resDefineAt` parameter to `ParseSpectraParam`
* add trimws() to detect if a column is numeric-like in `.Col2Numeric` function
* add `DenoiseSpectra` to `SpectraData` class

# SpectraTools 0.2.8
* add subsetting, `length` and `names` to `SpectraData` class

# SpectraTools 0.2.9
* add `ExportSpectra` (only `MSP` format supported currently) to `SpectraData` class

# SpectraTools 0.2.10
* add `detectIntensityAbs`, `thrDetectCounts` in `DenoiseSpectra` and `MatchParam`
* update `precursorMZ` in `DenoiseSpectra`
* bugfix for `ParseMGF` for different column numbers in  fragment records

# SpectraTools 0.2.11
* correct gnps score function to select the best fragment pair to calculate cosine score

# SpectraTools 0.2.12
* correct gnps score functions