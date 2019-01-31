site_index = commandArgs(trailingOnly = TRUE)

{
  if (length(site_index) != 1) {
    stop("Provide only one index")
  }
  
  
  library(dplyr)
  library(spBayes)
  library(ggplot2)
  
  frm = readr::read_csv("data/frm.csv") %>%
    mutate(
      day = (as.Date(date) - as.Date("2007/1/1")) %>% as.integer()
    ) %>%
    select(site, day, pm25) %>%
    arrange(site, day)
  
  sites = frm$site %>% unique()
  
  site_index = as.integer(site_index)
  
  stopifnot(!is.na(site_index))
  stopifnot(site_index %in% seq_along(sites))
  
  
  cur_site = sites[site_index]
  
  site_data = frm %>% filter(site == cur_site)
  
  lm = spLM(
    pm25 ~ 1, data = site_data, coords = cbind(x=site_data$day, y=0),
    starting = list(sigma.sq = 1,   tau.sq = 1,   phi = sqrt(3)/30),
    tuning   = list(sigma.sq = 0.1, tau.sq = 0.1, phi = 0.1),
    priors   = list(beta.Norm = list(0,1000), phi.Unif = c(sqrt(3)/30-1e-6, sqrt(3)/30+1e-6),
                    sigma.sq.IG = c(2, 2), tau.sq.IG = c(2, 0.1)),
    cov.model = "gaussian",
    amcmc = list(n.batch = 1000, batch.length = 50, accept.rate = 0.43)
  )
  
  n_iter = nrow(lm$p.theta.samples)
  start = n_iter/2+1
  thin = (n_iter/2)/2500
  
  day_pred = jitter(0:364,0.001)
  lm_pred = spPredict(
    lm, cbind(x = day_pred, y = 0), 
    pred.covars = matrix(1,nrow=365),
    start = start, thin = thin  
  )
  
  df_pred = tibble(
    day = day_pred,
    pm25_mean = apply(lm_pred$p.y.predictive.samples, 1, mean),
    pm25_med  = apply(lm_pred$p.y.predictive.samples, 1, median)
  )
  
  results_dir = file.path("results/",cur_site)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(site_data, file.path(results_dir, "data.rds"))
  saveRDS(lm,        file.path(results_dir, "splm.rds"))
  saveRDS(df_pred,   file.path(results_dir, "pred.rds"))
  
  pdf(file.path(results_dir, "pred.pdf"))
  print(
    ggplot(df_pred, aes(x=day, y=pm25_mean)) +
      geom_line() +
      geom_point(data = site_data, color="red", aes(y=pm25)) +
      geom_path(data = site_data, color="red", aes(y=pm25))
  )
  dev.off()
}