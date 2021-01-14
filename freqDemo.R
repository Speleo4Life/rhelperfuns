# freqDemo(x, cummulative = FALSE, relfreq = FALSE, cumrelfreq = FALSE, 
# decrease = TRUE, demoIterations = 5)
#
# An interactive demonstation of freq(), a simple helper function for generating ungrouped 
# frequency distributions with options to show cummulative frequency, relative frequency, 
# and cummulative relative frequency. This demo was designed for the UBC course PSCY278 to
# illustate for students the concept of a FOR loop, and to demonstrate what was happening
# under the hood of freq().
#
# Created By:
# Ray MacNeil, UBC Vision Lab
# raymond.macneil@utoronto.ca
# GitHub Repository: https://github.com/Speleo4Life/rhelperfuns
# January 14th, 2021
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
# demoIterations [numeric]: Argument specifies the number of iterations for which to
# show the output of the frequency counting solution implemented by freq(). 
#
# VALUE
# An object of class "data.frame"
#
exam_scores <- c(95, 57, 76, 93, 86, 80, 89, 76, 76, 63, 74, 94, 
                 96, 77, 65, 79, 60, 56, 72, 82, 70, 67, 79, 71, 77, 52, 76, 
                 68, 72, 88, 84, 70, 83, 93, 76, 82, 96, 87, 69, 89, 77, 81, 
                 87, 65, 77, 72, 56, 78, 78, 58, 54, 82, 82, 66, 73, 79, 86, 
                 81, 63, 46, 62, 99, 93, 82, 92, 75, 76, 90, 74, 67)


freqDemo <- function(x, cummulative = FALSE,
                 relfreq = FALSE,
                 cumrelfreq = FALSE,
                 decrease = TRUE,
                 demoIterations = 5) {
  
  scores <- unique(x)
  frequency <- rep(NaN, times = length(scores))
  scores <- sort(scores, decreasing = TRUE)
  x <- sort(x, decreasing = TRUE)
  
  print("All exam scores (descending): ")
  print(x)
  
  invisible(readline(prompt = "Press any key to continue... "))
  
  print("Unique exam scores: ")
  print(scores)
  
  invisible(readline(prompt = "Press any key to continue... "))
  
  for (ii in 1:length(scores)) {
    
    if (ii <= demoIterations) {
    
      equivalence <- (scores[ii] == x)
      invisible(readline(prompt = sprintf("See equivalence test for exam score %d 
                                          (press any key to continue)... ", 
                                          scores[ii])))
      print(equivalence)
      
      get.frequency <- sum(scores[ii] == x)
      invisible(readline(prompt = sprintf("See frequency count for exam score %d 
                                          (pres any key to continue)... ",
                                          scores[ii])))
      print(get.frequency)
      
      frequency[ii] <- get.frequency
      invisible(readline(prompt = "See assignment of frequency count for exam score  
                         %d (pres any key to continue)... ", scores[ii]))
      print(frequency) }
    
    else {
      get.frequency <- sum(scores[ii] == x)
      frequency[ii] <- get.frequency }
  }
  
  out <- cbind.data.frame(scores, frequency)
  
  if (cummulative) {
    out <- within(out, cum.frequency <- cumsum(frequency))
  }
  
  if (relfreq) {
    out <- within(out, rel.freq <- (frequency / sum(out$frequency)))
  }
  
  if (cumrelfreq & !relfreq) {
    out <- within(out, rel.freq <- (frequency / sum(out$frequency)))
    out <- within(out, cum.rel.freq <- cumsum(rel.freq))
  }
  
  else if (cumrelfreq & relfreq) {
    out <- within(out, cum.rel.freq <- cumsum(rel.freq))
  }
  
  row.names(out) <- NULL
  
  return(out)
}

freqDemo(x = exam_scores)


