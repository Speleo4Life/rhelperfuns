# freq(x, cummulative = FALSE, relfreq = FALSE, cumrelfreq = FALSE, decrease = TRUE)
#
# A simple helper function for generating ungrouped frequency distributions with options
# to show cummulative frequency, relative frequency, and cummulative relative frequency.
#
# Created By:
# Ray MacNeil, UBC Vision Lab
# raymond.macneil@utoronto.ca
# GitHub Repository: https://github.com/Speleo4Life/rhelperfuns
#
# ARGUMENTS
# x [numeric]: A vector of values for which the frequency table is desired.
#
# cummulative [logical]: If `TRUE`, output will show the cummulative frequency of elements
# contained in the vector x. Default is `FALSE`.
#
# relfreq [logical]: If `TRUE` output will display the relative frequency of each unique
# element contained in the vector x. Default is `FALSE`.
#
# cumrelfreq [logical]: If `TRUE` output will display the cummulative relative frequency
# of each unqiue element contained in the vector x. Default is `FALSE`.
#
# decrease [logical]: If `TRUE` (default) output will display frequency table in
# decreasing order with maximum value of x at the top and the minimum value of x
# at the bottom.
#
# VALUE
# An object of class "data.frame"
#

freq <- function(x, cummulative = FALSE,
                 relfreq = FALSE,
                 cumrelfreq = FALSE,
                 decrease = TRUE) {

  scores <- unique(x)
  count <- rep(NaN, times = length(scores))

  for (ii in 1:numel(scores)) {
    get.count <- sum(scores[ii] == x)
    count[ii] <- get.count }

  out <- cbind.data.frame(scores, count)
  out <- out[order(out$scores, decreasing = decrease),]

  if (cummulative) {
    out <- within(out, cum.count <- cumsum(count))
  }

  if (relfreq) {
    out <- within(out, rel.freq <- (count / sum(out$count)))
  }

  if (cumrelfreq & !relfreq) {
    out <- within(out, rel.freq <- (count / sum(out$count)))
    out <- within(out, cum.rel.freq <- cumsum(rel.freq))
  }

  else if (cumrelfreq & relfreq) {
    out <- within(out, cum.rel.freq <- cumsum(rel.freq))
  }

  row.names(out) <- NULL

  return(out)
}
