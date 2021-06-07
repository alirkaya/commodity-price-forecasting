arma.innovation <- function(x, arma.model, ar.truncation=10) {
  p <- arma.model$arma[1]
  q <- arma.model$arma[2]
  ar.coef <- arma.model$coef[seq_len(p)]
  ma.coef <- arma.model$coef[p + seq_len(q)]
  if (q == 0) {
    infinite.ar.coef <- ar.coef
  } else {
    infinite.ar.coef <- -ARMAtoMA(-ma.coef, -ar.coef, ar.truncation)
  }
  return(as.vector(filter(x, c(1, -infinite.ar.coef), side=1)))
}

# Example.
n <- 1000
epsilon <- rnorm(n)
x <- numeric(n)
for (i in seq(2,n)) {
  x[i] <- 0.8 * x[i-1] + epsilon[i] - 0.3 * epsilon[i-1]
}
model <- arima(x, order=c(1,0,1), include.mean=FALSE)
innovation <- arma.innovation(x, model)
max(abs(innovation - model$residual), na.rm=TRUE)
