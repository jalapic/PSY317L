
### A function for calculating the mode

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}
