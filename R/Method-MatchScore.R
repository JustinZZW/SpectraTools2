#' @export
setMethod(
  "length",
  "signature" = c("MatchScore"),
  function(x) {
    return(length(x@matchedFragments))
  }
)
