.ExportMSP <- function(spectra, exportFile) {
  col_rename <- c("PRECURSORMZ", "RETENTIONTIME")
  col_nms <- colnames(spectra@info)
  idx_rename <- match(c("mz", "rt"), tolower(col_nms))
  col_nms[na.omit(idx_rename)] <- col_rename[which(!is.na(idx_rename))]
  col_nms <- toupper(col_nms)
  row_nms <- rownames(spectra@info)
  info_list <- lapply(apply(spectra@info, 1, function(dr) list(paste(col_nms, dr, sep = ": "))), `[[`, 1)
  names(info_list) <- row_nms
  info_list <- na.omit(info_list)
  msp_data <- sapply(row_nms, function(nm) {
    info <- info_list[[nm]]
    spec <- spectra@spectra[[nm]]
    if (is.null(spec)) {
      return(NA)
    }
    spec_paste <- paste(apply(spec, 1, paste, collapse = ' '), collapse = "\n")
    res <- c(info, paste("Num Peaks:", nrow(spec)), spec_paste, "\n")
    return(paste(res, collapse = "\n"))
  })

  writeLines(msp_data, con = exportFile, sep = "\n")
}

.ExportMGF <- function(spectra, exportFile) {
  stop("Not implemented yet.")
}