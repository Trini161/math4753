#' Title Binomial Simulation
#' @param iter An integer specifying the number of iterations (or experiments) to run.
#' @param n An integer specifying the number of trials per iteration.
#' @param p A numeric value between 0 and 1 specifying the probability of success for each trial.
#'
#' @return A named vector of proportions showing the frequency of the number of successes
#' out of `n` trials over all `iter` iterations.
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @export
#' @examples myBin(iter = 100, n = 10, p = 0.7)
#'
myBin <- function(iter = 100, n = 10, p = 0.7) {
  # Create a matrix to hold the samples (initially filled with NA's)
  sam.mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Vector to hold the number of successes in each trial
  succ <- numeric(iter)

  for (i in 1:iter) {
    # Fill each column with a new sample
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    # Calculate the sum (number of successes)
    succ[i] <- sum(sam.mat[, i])
  }

  # Create a table of the number of successes
  succ.tab <- table(factor(succ, levels = 0:n))

  # Labels for the plot
  iter.lab <- paste0("iter = ", iter)
  n.lab <- paste0("n = ", n)
  p.lab <- paste0("p = ", p)
  lab <- paste(iter.lab, ", ", n.lab, ", ", p.lab)

  # Create the barplot of the proportions
  barplot(succ.tab / iter, col = rainbow(n + 1), main = "Binomial Simulation", sub = lab, xlab = "Number of successes")

  # Return the proportion table
  return(succ.tab / iter)
}
