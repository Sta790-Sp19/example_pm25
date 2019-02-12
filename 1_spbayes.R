{
  args = commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) %in% c(2,3))
  
  n_iter = args[1]
  n_parallel = args[2]
  if (!is.null(args[3]))
    n_cores = args[3]
  
  suppressMessages( library(dplyr) )
  suppressMessages( library(spBayes) )
  suppressMessages( library(ggplot2) )
  
  frm = suppressMessages(readr::read_csv("data/frm.csv")) %>%
    mutate(
      day = (as.Date(date) - as.Date("2007/1/1")) %>% as.integer()
    ) %>%
    select(site, day, pm25) %>%
    arrange(site, day)
  
  sites = frm$site %>% unique()
  
  
  site_index = 1
  
  cur_site = sites[site_index]
  
  site_data = frm %>% filter(site == cur_site)
  
  
  fit = function(i) {
    cat("Start iter", i, "\n")
    lm = spLM(
      pm25 ~ 1, data = site_data, coords = cbind(x=site_data$day, y=0),
      starting = list(sigma.sq = 1,   tau.sq = 1,   phi = sqrt(3)/30),
      tuning   = list(sigma.sq = 0.1, tau.sq = 0.1, phi = 0.1),
      priors   = list(beta.Norm = list(0,1000), phi.Unif = c(sqrt(3)/30-1e-6, sqrt(3)/30+1e-6),
                      sigma.sq.IG = c(2, 2), tau.sq.IG = c(2, 0.1)),
      cov.model = "gaussian",
      amcmc = list(n.batch = 1000, batch.length = 50, accept.rate = 0.43),
      verbose = FALSE
    )
    cat("End iter", i, "\n")
  }
  
  start = Sys.time()
  
  #z = parallel::mclapply(seq_len(n_iter), fit, mc.cores = n_parallel)
  z = fit(1)
  
  end = Sys.time()
  
  cat("Run time: ", difftime(end, start, units="secs"), "s\n", sep="")
}