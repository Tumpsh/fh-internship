library(msm)
library(dplyr)



get_hazard_matrix <- function(obs_years, b_years, cs, weights, pars, haz_rat){
  dens_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
  cdf_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))

  ## For each birth year, going along the list of observation years and inserting the modeled density
  ## and cumulative density into their respective matrices

  for(b_year in b_years){
    for(obs_year in obs_years){
      dens_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_density(the_time = obs_year,
                                                                                      birth_year = b_year,
                                                                                      cs = cs,
                                                                                      weights = weights,
                                                                                      pars = pars,
                                                                                      haz_rat = haz_rat)
      cdf_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_cdf(the_time = obs_year,
                                                                                 birth_year = b_year,
                                                                                 cs = cs,
                                                                                 weights = weights,
                                                                                 pars = pars,
                                                                                 haz_rat = haz_rat)
      print(paste(obs_year, b_year))
    }
  }
  haz_mat <- dens_mat/(1 - cdf_mat)
  return(haz_mat)
}

get_density_matrix <- function(obs_years, b_years, cs, weights, pars, haz_rat){
  dens_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
  cdf_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
  
  ## For each birth year, going along the list of observation years and inserting the modeled density
  ## and cumulative density into their respective matrices
  
  for(b_year in b_years){
    for(obs_year in obs_years){
      dens_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_density(the_time = obs_year,
                                                                                      birth_year = b_year,
                                                                                      cs = cs,
                                                                                      weights = weights,
                                                                                      pars = pars,
                                                                                      haz_rat = haz_rat)
      cdf_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_cdf(the_time = obs_year,
                                                                                 birth_year = b_year,
                                                                                 cs = cs,
                                                                                 weights = weights,
                                                                                 pars = pars,
                                                                                 haz_rat = haz_rat)
    }
  }
  haz_mat <- dens_mat/(1 - cdf_mat)
  return(dens_mat)
}


get_cdf_matrix <- function(obs_years, b_years, cs, weights, pars, haz_rat){
  dens_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
  cdf_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
  
  ## For each birth year, going along the list of observation years and inserting the modeled density
  ## and cumulative density into their respective matrices
  
  for(b_year in b_years){
    for(obs_year in obs_years){
      dens_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_density(the_time = obs_year,
                                                                                      birth_year = b_year,
                                                                                      cs = cs,
                                                                                      weights = weights,
                                                                                      pars = pars,
                                                                                      haz_rat = haz_rat)
      cdf_mat[as.character(b_year), as.character(obs_year)] <- find_weighted_cdf(the_time = obs_year,
                                                                                 birth_year = b_year,
                                                                                 cs = cs,
                                                                                 weights = weights,
                                                                                 pars = pars,
                                                                                 haz_rat = haz_rat)
    }
  }
  haz_mat <- dens_mat/(1 - cdf_mat)
  return(cdf_mat)
}

# age_adjust <- function(year, rates, groupings){
#   rates <- rates %>% mutate(age = year - b_year)
#   specrates <- mapply(FUN = function(low, high, dat){
#     tdat <- dat[dat$age %in% low:high, ]
#     return(mean(tdat[, as.character(year)]))
#   }, low = groupings$low, high = groupings$high, MoreArgs = list(dat = rates))
#   
#   weighted <- groupings$rate*specrates
#   weighted[is.na(weighted)] <- 0
#   return(sum(weighted))
# }
# 
# hazz <- get_hazard_matrix(seq(1975, 2014, 5), seq(1880, 2010, 5), cs = wh_weights$c, weights = wh_weights$weight, pars = wh_pars, haz_rat = 0.6002379)
# bl_hazz <- get_hazard_matrix(seq(1975, 2014, 5), seq(1880, 2010, 5), cs = bl_weights$c, weights = bl_weights$weight, pars = bl_pars, haz_rat = 0.8401731)
# haz_dat <- as.data.frame(hazz) %>% tibble::rownames_to_column() %>% mutate(b_year = as.numeric(rowname))
# bl_haz_dat <- as.data.frame(bl_hazz) %>% tibble::rownames_to_column() %>% mutate(b_year = as.numeric(rowname))

# listy <- lapply(seq(1975, 2014, 5), function(year){age_adjust(year = year, rates = select(haz_dat, one_of(c(as.character(year), "b_year"))), groupings = stdpop)})
# bl_listy <- lapply(seq(1975, 2014, 5), function(year){age_adjust(year = year, rates = select(bl_haz_dat, one_of(c(as.character(year), "b_year"))), groupings = stdpop)})
# 

# 
# age_adjust <- function(haz_mat){
#   suppressWarnings(source("2000-std-pop.R"))
#   

#   
# 
#   for(obs_year in as.numeric(colnames(haz_mat))){
# 
#     stdpop <- stdpop %>% ddply(c("age"), mutate, newyear = mean(haz_dat[haz_dat$b_year %in% low:high, as.character(obs_year)]))
#     names(stdpop)[names(stdpop) == "newyear"] <- as.character(obs_year)
# 
#   }
#   stdpop[is.na(stdpop)] <- 0
#   return(stdpop)
# }

# # ## EXAMPLE
# hazz <- get_hazard_matrix(obs_years=1975:2014, b_years=(1915:2014), cs = wh_weights$c, weights = wh_weights$weight, pars = wh_pars, haz_rat = 0.6002379)
#  # plot(hazz["1924",])
# bl_hazz <- get_hazard_matrix(obs_years=1975:2014, b_years=(1915:2014), cs = bl_weights$c, weights = bl_weights$weight, pars = bl_pars, haz_rat = 0.8401731)
# 
#  
 
get_rate_matrices <- function(dat){
  
## This function returns a list of matrices with the rates of transitioning states.
## Done by taking in a data frame with incidence rate and survival rates as two columns, 
## since this format is easily made with our SEER data.
##
## Each matrix represents one age group's transition rates. We are assuming age-dependent
## incidence and non-age-dependent survival for our model.
      
  mapply(FUN = function(inc, surv){
    matrix(c(-inc, inc, 0, 0, -surv, surv, 0, 0, 0), nrow = 3, byrow = TRUE)
  }, inc = dat$incrate, surv = dat$survrate, SIMPLIFY = FALSE)
}

find_prob_matrix <- function(start_time, end_time, breakpts, rates){
  
## Returns modeled probabilities of transfering between states within a timeframe,
## given age-dependent rates (only incidence is age dependent in our example)
    
  marked_times <- sort(c(breakpts, start_time, end_time))
  marked_times <- marked_times[marked_times >= start_time & marked_times <= (end_time)]
  
## Maps each relevant time period (start time to first breakpoint, in between each 
## breakpoint in the observation time, and last breakpoint to end time) to the rate
## matrix which should be used for that time period
  
  mapping <- approx(x = breakpts, 
                    y = 1:length(breakpts),
                    xout = marked_times, 
                    method = "constant",
                    rule = 2)
  lengths_spent <- diff(mapping$x)
  ind_matrices <- mapping$y[-(length(mapping$y))]
  
## Matrix exponential of rate matrix multiplied by time spent in that age group will
## give probability matrix of transitioning between states within that time period.
##
## Matrix multiplication of all the subsequent probability matrices will give one total
## for the whole interval.
    
  rates_list <- mapply(MatrixExp, rates[ind_matrices], lengths_spent, SIMPLIFY = FALSE)
  mat <- matrix(0, nrow(rates[[1]]), ncol(rates[[1]]))
  diag(mat) <- 1
  for(new_mat in rates_list){
    mat <- mat %*% new_mat
  }
  return(mat)
}


# # EXAMPLE:
# start_time <- 24
# end_time <- 84
# breakpts <- c(0, 30, 60, 90)
# a <- matrix(c(-1/3, 1/3, 0, 0, -1/4, 1/4, 0, 0, 0), nrow = 3, byrow = TRUE)
# b <- matrix(c(-1/3, 1/3, 0, 0, -1/4, 1/4, 0, 0, 0), nrow = 3, byrow = TRUE)
# c <- matrix(c(-1/2, 1/2, 0, 0, -1/4, 1/4, 0, 0, 0), nrow = 3, byrow = TRUE)
# d <- matrix(c(-1/2, 1/2, 0, 0, -1/4, 1/4, 0, 0, 0), nrow = 3, byrow = TRUE)
# rates <- list(a, b, c, d)
# ratesnew <- list(a*0.6, b*0.6, c*0.6, d*0.6)
# listy <- lapply(seq(0,800,1), FUN = function(iter){find_prob_matrix(start_time=0,end_time=iter,breakpts = breakpts,rates=rates)})
# q <-lapply(listy, "[[", i=1, j=3)
# 
# p <- c(unlist(lapply(listy, "[[", i=1, j=1))[1:60]*1/3,unlist(lapply(listy, "[[", i=1, j=1))[61:800]*1/2)
# da <- data.frame(p=p[1:799], q=unlist(q)[1:799])
# da <- da %>% mutate(haz = p/(1-q))
# plot(da$haz)

find_weighted_density <- function(the_time, birth_year, cs, weights, pars, haz_rat){
  
  breakpts <- pars$age
  oldrates <- pars %>% get_rate_matrices()
  newrates <- pars %>% mutate(survrate = haz_rat*survrate) %>% get_rate_matrices()
  
  many_densities <- mapply(FUN = function(c, weight){
    return(find_density(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  many_cdfs <- mapply(FUN = function(c, weight){
    return(find_cdf(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  pdf <- sum(many_densities)
  return(pdf)
  
}

find_weighted_cdf <- function(the_time, birth_year, cs, weights, pars, haz_rat){
  
  breakpts <- pars$age
  oldrates <- pars %>% get_rate_matrices()
  newrates <- pars %>% mutate(survrate = haz_rat*survrate) %>% get_rate_matrices()
  
  many_densities <- mapply(FUN = function(c, weight){
    return(find_density(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  many_cdfs <- mapply(FUN = function(c, weight){
    return(find_cdf(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  cdf <- sum(many_cdfs)
  return(cdf)
  
}

find_weighted_hazard <- function(the_time, birth_year, cs, weights, pars, haz_rat){
  
  breakpts <- pars$age
  oldrates <- pars %>% get_rate_matrices()
  newrates <- pars %>% mutate(survrate = haz_rat*survrate) %>% get_rate_matrices()
  
  many_densities <- mapply(FUN = function(c, weight){
    return(find_density(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  many_cdfs <- mapply(FUN = function(c, weight){
    return(find_cdf(the_time, birth_year, c, breakpts, oldrates, newrates)*weight)
  }, c = cs, weight = weights)
  
  haz <- sum(many_densities)/(1 - sum(many_cdfs))
  return(haz)
  
}

# # EXAMPLE:
# find_weighted_hazard(the_time = 60, birth_year = 0, cs = c(61, 75, 89), weights = c(0.1, 0.5, 0.4), pars = wh_parts, haz_rat = 1)
# listy <- lapply(1920:2000, FUN = "find_weighted_hazard", birth_year = 1920, cs = wh_weights$c, weights = wh_weights$weight, pars = wh_pars, haz_rat = 45)
# plot(unlist(listy))


find_density <- function(the_time, birth_year, c, breakpts, oldrates, newrates){
  
## Gives value of the density function of dying at a certain time. This gives
## the derivative of the result of the find_cdf function, which is a mixture 
## of probability of dying before treatment introduction, being diagnosed before
## treatment introduction but dying after, and being diagnosed and dying both after
## treatment introduction.
    
  pmat_c <- find_prob_matrix(start_time = 0, 
                            end_time = c - birth_year,  
                            breakpts = breakpts,
                            rates = oldrates)
  pmat_t <- find_prob_matrix(start_time = 0, 
                            end_time = the_time - birth_year,  
                            breakpts = breakpts,
                            rates = oldrates)
  pmat_t_c <- find_prob_matrix(start_time = c - birth_year, 
                              end_time = the_time - birth_year,  
                              breakpts = breakpts,
                              rates = oldrates)
  pmat_t_c_new <- find_prob_matrix(start_time = c - birth_year, 
                                  end_time = the_time- birth_year,  
                                  breakpts = breakpts,
                                  rates = newrates)
  ind <- approx(x = breakpts, 
                    y = 1:length(breakpts),
                    xout = (the_time - birth_year), 
                    method = "constant",
                    rule = 2)
  oldrate <- oldrates[[ind$y]]
  newrate <- newrates[[ind$y]]
  if(the_time<c){
    dens=pmat_t[1,2]*oldrate[2,3]  
  }else{
    dens=pmat_c[1,2]*pmat_t_c[2,2]*oldrate[2,3]+pmat_c[1,1]*pmat_t_c_new[1,2]*newrate[2,3]
  }
  return(dens)
}

find_cdf <- function(the_time, birth_year, c, breakpts, oldrates, newrates){
  
## Returns the probability of dying before a certain point in time, a function which 
## is a mixture of the probability of dying before treatment introduction, being
## diagnosed before treatment introduction but dying after, and being diagnosed and 
## dying after treatment introduction.
  
  pmat_c <- find_prob_matrix(start_time = 0, 
                            end_time = c - birth_year,  
                            breakpts = breakpts,
                            rates = oldrates)
  pmat_t <- find_prob_matrix(start_time = 0, 
                            end_time = the_time - birth_year,  
                            breakpts = breakpts,
                            rates = oldrates)
  pmat_t_c <- find_prob_matrix(start_time = c - birth_year, 
                              end_time = the_time - birth_year,  
                              breakpts = breakpts,
                              rates = oldrates)
  pmat_t_c_new <- find_prob_matrix(start_time = c - birth_year, 
                                  end_time = the_time- birth_year,  
                                  breakpts = breakpts,
                                  rates = newrates)
  ind <- approx(x = breakpts, 
                y = 1:length(breakpts),
                xout = (the_time - birth_year), 
                method = "constant",
                rule = 2)
  oldrate <- oldrates[[ind$y]]
  newrate <- newrates[[ind$y]]
  
  if(the_time<c){
    cdf=pmat_t[1,3]
  }else{
    cdf=pmat_c[1,3]+pmat_c[1,2]*pmat_t_c[2,3]+pmat_c[1,1]*pmat_t_c_new[1,3]
  }
  return(cdf)
}


## OLD FUNCTION WITH ONLY POINT CHANGE IN TREATMENT 
#get_hazard_matrix <- function(obs_years, b_years, c, pars, haz_rat){
# 
# ## Function returns matrix with each row representing one birth year, and each column representing 
# ## an observation year, returning the mortality hazard for someone, who was born in year x, at year y
# 
# ## Mortality is represented as the ratio between density of failure and survival (1 - cumulative dens.).
# ## Both of these matrices are useful, and thus are kept here in case we choose to use them in a later 
# ## function.
# 
# dens_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
# cdf_mat <- matrix(ncol = length(obs_years), nrow = length(b_years), dimnames = list(b_years, obs_years))
# 
# ## These objects (breakpts, oldrates, newrates) are used in the find_density and find_cdf functions, 
# ## explained in their definition
# 
# breakpts <- pars$age
# oldrates <- pars %>% get_rate_matrices()
# newrates <- pars %>% mutate(survrate = haz_rat*survrate) %>% get_rate_matrices()
# 
# ## For each birth year, going along the list of observation years and inserting the modeled density
# ## and cumulative density into their respective matrices
# 
# for(b_year in b_years){
#   for(obs_year in obs_years){
#     dens_mat[as.character(b_year), as.character(obs_year)] <- find_density(the_time = obs_year, 
#                                                                            birth_year = b_year, 
#                                                                            c = c, 
#                                                                            breakpts = breakpts, 
#                                                                            oldrates = oldrates, 
#                                                                            newrates = newrates)
#     cdf_mat[as.character(b_year), as.character(obs_year)] <- find_cdf(the_time = obs_year, 
#                                                                       birth_year = b_year, 
#                                                                       c = c, 
#                                                                       breakpts = breakpts, 
#                                                                       oldrates = oldrates, 
#                                                                       newrates = newrates)
#   }
# }
# haz_mat <- dens_mat/(1-cdf_mat)
# return(haz_mat)
# }




## Old functions used before age-dependent incidence was added 

# 
# observe_pop <- function(lower, upper, c, oldrate, newrate){
#   d <- data.frame(x=seq(lower, upper),
#                   cdf=unlist(lapply(seq(lower, upper), FUN="find_cdf", c=c, oldrate=oldrate, newrate=newrate)),
#                   pdf=unlist(lapply(seq(lower, upper), FUN="find_density", c=c,oldrate=oldrate, newrate=newrate)))
#   d <- d %>% mutate(haz=pdf/(1-cdf))
#   return(d)
# }
# 
# compare_pops <- function(lower, upper, c, loldrate, lnewrate, holdrate, hnewrate){
#   d <- data.frame(x=seq(lower, upper),
#                   lcdf=unlist(lapply(seq(lower, upper), FUN="find_cdf", c=c, oldrate=loldrate, newrate=lnewrate)),
#                   lpdf=unlist(lapply(seq(lower, upper), FUN="find_density", c=c,oldrate=loldrate, newrate=lnewrate)),
#                   hcdf=unlist(lapply(seq(lower, upper), FUN="find_cdf", c=c, oldrate=holdrate, newrate=hnewrate)),
#                   hpdf=unlist(lapply(seq(lower, upper), FUN="find_density", c=c,oldrate=holdrate, newrate=hnewrate)))
#   
#   d <- d %>% mutate(lhaz=lpdf/(1-lcdf),
#                     hhaz=hpdf/(1-hcdf),
#                     rat=hhaz/lhaz,
#                     diff=hhaz-lhaz)
#   return(d)
# }
