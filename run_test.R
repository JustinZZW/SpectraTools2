devtools::document("E:/Work/dev_r/SpectraTools/SpectraTools")
devtools::load_all("E:/Work/dev_r/SpectraTools/SpectraTools")


load("D:/TmpData/met4dx_demo_lip/xx.rda")
matchScore = readRDS("D:/TmpData/met4dx_demo_lip/mm.rda")
pd <- PlotMirror(matchScore, 'x', plotPDF=FALSE, plotPNG=FALSE)

param <- SpectraTools::ParseSpectraParam(
  type = "mgf",
  denoise = FALSE,
  includePrecursor = TRUE,
  ppmPrecursorFilter = 20,
  thrIntensityAbs = 30,
  thrIntensityRel = 0.01,
  labelKeep = c("PEPMASS", "RTINSECONDS", "TITLE", "RAWSCANS"),
  labelName = c("precursor_info", "rt", "info", "raw_scans"),
  resDefineAt = 200
)
data_file = "D:/TmpData/met4dx_demo_lip/plasma1_1_P1-D7_1_4153.d/plasma1_1_P1-D7_1_4153_5.3.236.mgf"
x = ParseSpectra(param, data_file)
MatchSpectra(spec_ref, spec_cad, param)
