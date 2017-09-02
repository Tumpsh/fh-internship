library(dplyr)
library(ggplot2)
library(plyr)

dat <- read.csv("TreatmentTypeRace.csv", stringsAsFactors = FALSE)
names(dat) <- c("race", "rt", "rp", "year", "rate", "count", "pop")

dat <- dat %>% filter(race %in% c("All races", "Black", "White"),
                      nchar(year) == 4)

# For each race and year, we must find the total cases, with or
# without treatment
dat <- ddply (dat, c("year", "race"), mutate, yearsum = sum(count))
dat <- dat %>% mutate(rt = (rt=="RT"),
                      rp = (rp=="RP"),
                      treat = ifelse(rt,
                                     ifelse(rp, "Both", "Radiation"),
                                     ifelse(rp, "Prostatectomy", "Neither")),
                      year = as.numeric(year))

# Dividing number with each category of treatment by the total
# cases will give the rate of each category per case
sumdat <- ddply(dat, c("year", "race", "treat"), summarise, rate = mean(count/yearsum))

# Splitting the plot by race into lines depicting the rate per
# case of each treatment category over time
# gg <- ggplot(sumdat %>% filter(treat == "Neither"), aes(x = year, y = 1 - rate, color = race))
# gg <- gg + facet_grid(treat~.)
# gg <- gg + geom_line()
# print(gg)

# Modeling a cumulative probability to fit the uptake gradient    
# before the year where it seems to level off, and finding a level
# after that year when the uptake seems to become constant
sumdat_any <- sumdat %>% filter(treat=="Neither") %>% 
                         mutate(treat = "either",
                                rate = 1-rate)

# Data by race in the interim period of uptake
bl_dat_inter <- sumdat_any %>% filter(race=="Black", year<=1992)
wh_dat_inter <- sumdat_any %>% filter(race=="White", year<=1992)

# Data when the uptake levels to a constant
dat_const <- sumdat_any %>% filter(race=="All races", year>1992)

# Making loess models assumed to be approximately the cdf of interim
# uptake
bl_loess <- loess(data=bl_dat_inter, rate~year)$fitted
wh_loess <- loess(data=wh_dat_inter, rate~year)$fitted

# Fraction of population which never will get treatment (C=infinity)
weight_never <- 1 - mean(dat_const$rate)

# Finding interim weights and scaling it so the total mass is 1
bl_loess_weights <- diff(bl_loess)*
                    (1 - weight_never)/
                    sum(diff(bl_loess))

bl_weights <- data.frame(c = 0, weight = 0)
bl_weights <- bl_weights %>% rbind(data.frame(c = 1983:1991, 
                                              weight = bl_loess_weights))
bl_weights <- bl_weights %>% rbind(data.frame(c = 10000, weight = weight_never))

wh_loess_weights <- diff(wh_loess)*
                    (1 - weight_never)/
                    sum(diff(wh_loess))

wh_weights <- data.frame(c = 0, weight = 0)
wh_weights <- wh_weights %>% rbind(data.frame(c = 1983:1991, 
                                              weight = wh_loess_weights))
wh_weights <- wh_weights %>% rbind(data.frame(c = 10000, weight = weight_never))

# Finding probability mass/density function of C values
# black:
# p(C=c) = { ~0  for c = -inf
#            scaled diff(bl_loess) for 1983<=c<=1992
#            ~0.316124 for c=inf
# scale the middle term so the total is 1
# white:
# p(C=c) = { ~0  for c = -inf
#            scaled diff(wh_loess) for 1983<=c<=1992
#            ~0.316124 for c=inf
# scale the middle term so the total is 1


# Finding specifically linear model (and thus uniform C distrib)
# bl_uptake_mod <- lm(data=bl_dat_mod, rate ~ year)
# wh_uptake_mod <- lm(data=wh_dat_mod, rate ~ year)
# gg <- ggplot(sumdat_any, aes(x=year, y=rate, color=race))
# gg <- gg+geom_line()
# gg <- gg+geom_smooth(method="loess")
# gg <- gg+geom_abline(intercept=bl_uptake_mod$coefficients[1], slope=bl_uptake_mod$coefficients[2])
# gg <- gg+geom_abline(intercept=wh_uptake_mod$coefficients[1], slope=wh_uptake_mod$coefficients[2])
# print(gg)

