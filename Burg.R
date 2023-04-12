burg <- function(x, p){
  x <- x - mean(x)
  den <- 2*sum(x^2)
  f <- b <- x
  a <- 0
  n <- length(x)
  var <- den/(2*n)
  
  for(k in 1:p){
    num <- -sum(f[2:n] * b[1:(n-1)])
    den <- (1 - a[k]^2)*den - f[1]^2 - b[n]^2
    
    a[k+1] <- 2*num/den
    
    ff <- f
    f <- f[2:n] + a[k + 1]*b[1:(n - 1)]
    b <- b[1:(n - 1)] + a[k + 1]*ff[2:n]
    if (k > 1){
      a_prima <- a
      for (j in 2:(k)){
        a[j] <- a_prima[j] + a[k + 1]*(rev(a_prima)[j])
      }
    }
    var[k + 1] <- var[k] * (1 - a[k + 1]^2)
    n <- length(f)
  }
  a <- a[-1]
  return(list(phi = a, var_a = var[k + 1]))
}

set.seed(1205)
x <- arima.sim(list(ar = c(0.84, -0.30, -0.45)), n = 20000)
benchr::benchmark(burg(x, p = 3), times = 1000)
benchr::benchmark(ar.burg(x, aic = FALSE, order.max = 3), times = 1000)
burg(x, p = 3)
b_x <- ar.burg(x, aic = FALSE, order.max = 3); b_x$ar; b_x$var.pred
    
