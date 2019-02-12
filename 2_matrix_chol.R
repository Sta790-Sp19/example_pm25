{
  args = commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2)

  n_iter = args[1]
  n_parallel = args[2]

  set.seed(100)
  
  exp_cov = function(d, sigma2=1, l=1, tau2=0.1) {
    stopifnot(nrow(d) == ncol(d))
    sigma2*exp(-(d*l)^2) + diag(tau2, nrow(d))
  }
  
  d = dist( 
    data.frame(
      x = runif(10000),
      y = runif(10000)
    )
  ) 
  
  cov = exp_cov(as.matrix(d))
  
  start = Sys.time()
  
  parallel::mclapply(
    seq_len(niter), 
    function(x) {
      system.time(chol(cov))
    }, 
    mc.cores = n_parallel
  )
  
  end = Sys.time()
  cat("Run time: ", difftime(end, start, units="secs"), "s\n", sep="")
}