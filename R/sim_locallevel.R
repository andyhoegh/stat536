#' sim_locallevel
#'
#' This function simulates data from a DLM with a local level
#' @param num.pts - number of time points
#' @param V - observation variance
#' @param W - evolution variance
#' @param mu.0 - starting point for state: default is 0
#' @return y - observed value
#' @return  mu - latent state
#' @export

sim_locallevel <- function(num.pts, V, W, mu.0){
  y <- mu <- rep(mu.0, num.pts)

  for (time.pts in 2:num.pts){
    mu[time.pts] <- mu[time.pts - 1] + rnorm(1, mean = 0, sd = sqrt(W))
    y[time.pts] <- rnorm(1, mean = mu[time.pts], sd = sqrt(V))
  }
  return(list(y = y, mu = mu))
}

