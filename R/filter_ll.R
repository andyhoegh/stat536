#' filter_ll
#'
#' This function simulates data from a DLM with a local level
#' @param y - observed time seris
#' @param mu0 - starting point for state
#' @param C0 - starting variance for state
#' @param V - observation variance
#' @param W - evolution variance
#' @return a - evolution state mean
#' @return R - evolution state variance
#' @return f - predictive dist mean
#' @return Q - predictive dist variance
#' @return mu - filtered state mean
#' @return C - filtered state variance
#' @export

filter_ll <- function(y, mu0, C0, V, W){
  num.pts <- length(y)
  a <- R <- f <- Q <- mu <- C <- rep(0, num.pts)

  # evolution dist parameters (theta_t|y_{t-1})
  a[1] <- mu0
  R[1] <- C0 + W

  # predictive dist parameters (y_t|y_{t-1})
  f[1] <- a[1]
  Q[1] <- R[1] + V

  # filtering dist parameters (theta_{t}|y_t)
  mu[1] <- a[1] + (R[1] / (R[1] + V)) * (y[1] - f[1])
  C[1] <- R[1] - (R[1]/ (R[1] + V)) * R[1]

  for (time.pts in 2:num.pts){
    # evolution dist parameters (theta_t|y_{t-1})
    a[time.pts] <- mu[time.pts - 1]
    R[time.pts] <- C[time.pts - 1] + W

    # predictive dist parameters (y_t|y_{t-1})
    f[time.pts] <- a[time.pts]
    Q[time.pts] <- R[time.pts] + V

    # filtering dist parameters (theta_{t}|y_t)
    mu[time.pts] <- a[time.pts] + (R[time.pts] / (R[time.pts] + V)) * (y[time.pts] - f[time.pts])
    C[time.pts] <- R[time.pts] - (R[time.pts]/ (R[time.pts] + V)) * R[time.pts]
  }
  return(list(a = a, R = R, f = f, Q = Q, mu = mu, C = C))
}
