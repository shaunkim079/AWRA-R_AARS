\name{performance.stats}
\alias{performance.stats}
\title{Compute performace statistics from an awra-r simulation (see \link{awrar.run})}
\usage{
  performance.stats(obs, run, start,
    start.water.year = "07-01")
}
\arguments{
  \item{obs}{Observed daily streamflow time series. Data
  frame with 2 columns named day (date) and flow (flow in
  cumecs)}

  \item{run}{A run obtained from \link{awrar.run}}

  \item{start}{Start date of the simulation}

  \item{start.water.year}{Start of the water year with
  format MM-DD}
}
\value{
  a data frame containing the statistics
}
\description{
  The statistics are \itemize{ \item bias: \eqn{B =
  \frac{|\sum_i q_i - \sum_i \hat{q}_i|}{\sum_i q_i}} \item
  max.yearly.bias: \eqn{BM = \max_k\left(\frac{|\sum_{i\in
  y_k} q_i - \sum_{i\in y_k} \hat{q}_i|}{\sum_{i\in y_k}
  q_i}\right)} with k the year number.  \item nse: \eqn{NSE
  = 1- \frac{\sum_i (q_i- \hat{q}_i)^2}{\sum_i
  (q_i-\bar{q})^2}} \item nse.monthly: NSE on monthly flows
  \item nse.log: daily NSE on log transformed flow where
  \eqn{logq = \log(q+10^{-4})} with q in m3/s. }
}
\examples{
##
}

