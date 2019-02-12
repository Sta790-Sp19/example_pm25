{
  args = commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2)
  
  n_iter = args[1]
  n_parallel = args[2]
  
  z=matrix(1, 10000, 10000)
  
  start = Sys.time()
  
  parallel::mclapply(
    seq_len(n_iter), 
    function(x) {
      system.time(z%*%z)
    }, 
    mc.cores = 1
  )
  
  end = Sys.time()
  cat("Run time: ", difftime(end, start, units="secs"), "s\n", sep="")
}