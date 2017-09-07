library(dplyr)
library(plyr)
library(ggplot2)
source("defining-simulation-functions.R")
source("real-world-age-specific-mortality.R")
source("real-world-parameter-dataframe.R")
source("real-world-uptake-trends.R")

# # RETURNS 0.4624242 (0.6893223 when not selecting for age) - OBJECTIVE: 2737178 (5151087 when not selecting) - NO NEED TO RUN
# opwh <- optimise(function(rat){
# 
#   print(rat)
#   sagemort <- agemort %>% filter(race == "White", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   sagemort <- sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   sagemort <- ddply(sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = wh_weights$c,
#                                                                                           weights = wh_weights$weight,
#                                                                                           pars = wh_pars,
#                                                                                           haz_rat = rat))
#   sagemort <- sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
# #  sagemort <- sagemort %>% mutate(weights = (1991 - yearbth))
# 
# #  sagemort <- sagemort %>% mutate(sumsq = pop*(mod - rate/100000)^2)
# 
#   print(sum(sagemort$loglikelihood))
#   return(sum(sagemort$loglikelihood))
# },
# interval = c(0, 2),
# maximum = TRUE)



# # RETURNS -424995.5 NO NEED TO RUN
# heswh <- numDeriv::hessian(function(rat){
# 
#   print(rat)
#   sagemort <- agemort %>% filter(race == "White", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   sagemort <- sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   sagemort <- ddply(sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = wh_weights$c,
#                                                                                           weights = wh_weights$weight,
#                                                                                           pars = wh_pars,
#                                                                                           haz_rat = rat))
#   sagemort <- sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
#   #  sagemort <- sagemort %>% mutate(weights = (1991 - yearbth))
# 
#   #  sagemort <- sagemort %>% mutate(sumsq = pop*(mod - rate/100000)^2)
# 
#   print(sum(sagemort$loglikelihood))
#   return(sum(sagemort$loglikelihood))
# }, x = opwh$maximum)



# # RETURNS 0.8666802 (1.108329 without age selection), OBJECTIVE: 515204.6 (799494.6 when not selecting) when not weighted for Pop- NO NEED TO RUN
# opbl <- optimise(function(rat){
# 
#   print(rat)
#   sagemort <- agemort %>% filter(race == "Black", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   sagemort <- sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   sagemort <- ddply(sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = bl_weights$c,
#                                                                                           weights = bl_weights$weight,
#                                                                                           pars = bl_pars,
#                                                                                           haz_rat = rat))
#  sagemort <- sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
# #  sagemort <- sagemort %>% mutate(sumsq = pop*(mod - rate/100000)^2)
# 
#   print(sum(sagemort$loglikelihood))
#   return(sum(sagemort$loglikelihood))
# },
# interval = c(0, 2),
# maximum = TRUE)
# 
# print(opbl)



# # RETURNS -21985.66 NO NEED TO RUN
# hesbl <- numDeriv::hessian(function(rat){
# 
#   print(rat)
#   sagemort <- agemort %>% filter(race == "Black", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   sagemort <- sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   sagemort <- ddply(sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = bl_weights$c,
#                                                                                           weights = bl_weights$weight,
#                                                                                           pars = bl_pars,
#                                                                                           haz_rat = rat))
#   sagemort <- sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
#   #  sagemort <- sagemort %>% mutate(weights = (1991 - yearbth))
# 
#   #  sagemort <- sagemort %>% mutate(sumsq = pop*(mod - rate/100000)^2)
# 
#   print(sum(sagemort$loglikelihood))
#   return(sum(sagemort$loglikelihood))
# }, x = opbl$maximum)
# 


# # RETURNS 0.5211818, OBJECTIVE 3250294, NO NEED TO RUN:
# opcb <- optimise(function(rat){
# 
#   print(rat)
#   bl_sagemort <- agemort %>% filter(race == "Black", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   bl_sagemort <- bl_sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   bl_sagemort <- ddply(bl_sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = bl_weights$c,
#                                                                                           weights = bl_weights$weight,
#                                                                                           pars = bl_pars,
#                                                                                           haz_rat = rat))
#   bl_sagemort <- bl_sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
#   wh_sagemort <- agemort %>% filter(race == "White", yearbth > 1905, yearbth < 1965, yeardth > 1983, age < 80)
#   wh_sagemort <- wh_sagemort %>% mutate(id = paste(yearbth, yeardth),
#                                   rat = rat)
#   wh_sagemort <- ddply(wh_sagemort, c("yearbth", "yeardth"), mutate, mod = find_weighted_hazard(the_time = yeardth,
#                                                                                           birth_year = yearbth,
#                                                                                           cs = wh_weights$c,
#                                                                                           weights = wh_weights$weight,
#                                                                                           pars = wh_pars,
#                                                                                           haz_rat = rat))
#   wh_sagemort <- wh_sagemort %>% mutate(loglikelihood = ifelse(mod==0, 0, - mod*pop + count*log(mod*pop)))
# 
#   #  sagemort <- sagemort %>% mutate(sumsq = pop*(mod - rate/100000)^2)
#   sagemort <- rbind(wh_sagemort, bl_sagemort)
# 
#   print(sum(sagemort$loglikelihood))
#   return(sum(sagemort$loglikelihood))
# },
# interval = c(0, 2),
# maximum = TRUE)





##### ANALYSES TO WRITE UP IN WRITEUP
# (515204.6-2737178)-3250294 = 2088.6 is the difference in log likelihood
# 2*2088.6 = 4177.2
# P(chisq >= 4177.2) with one degree of freedom = 0 basically
# Indicates our disparate efficacy model is better than the same efficacy model
# 
# 1/hessian value for each black and white log likelihoods is an estimate
# of the variance in the parameter
# For whites, 0.001533938
# For blacks, 0.006744197




# ## CREATING AGEMORT
# 
# 
# agemort <- ddply(agemort, c("yearbth", "yeardth", "race"), mutate, mod = ifelse(race == "White",
#                                                                                 find_weighted_hazard(the_time = yeardth,
#                                                                                                      birth_year = yearbth,
#                                                                                                      cs = wh_weights$c,
#                                                                                                      weights = wh_weights$weight,
#                                                                                                      pars = wh_pars,
#                                                                                                      haz_rat = 0.4624242),
#                                                                                 find_weighted_hazard(the_time = yeardth,
#                                                                                                      birth_year = yearbth,
#                                                                                                      cs = bl_weights$c,
#                                                                                                      weights = bl_weights$weight,
#                                                                                                      pars = bl_pars,
#                                                                                                      haz_rat = 0.8666802)))
# 
# agemort <- ddply(agemort, c("yearbth", "yeardth", "race"), mutate, modnotreat = ifelse(race == "White",
#                                                                                        find_weighted_hazard(the_time = yeardth,
#                                                                                                             birth_year = yearbth,
#                                                                                                             cs = wh_weights$c,
#                                                                                                             weights = wh_weights$weight,
#                                                                                                             pars = wh_pars,
#                                                                                                             haz_rat = 1),
#                                                                                        find_weighted_hazard(the_time = yeardth,
#                                                                                                             birth_year = yearbth,
#                                                                                                             cs = bl_weights$c,
#                                                                                                             weights = bl_weights$weight,
#                                                                                                             pars = bl_pars,
#                                                                                                             haz_rat = 1)))
# 
# agemort <- ddply(agemort, c("yearbth", "yeardth", "race"), mutate, modcrazytreat = ifelse(race == "White",
#                                                                                        find_weighted_hazard(the_time = yeardth,
#                                                                                                             birth_year = yearbth,
#                                                                                                             cs = wh_weights$c,
#                                                                                                             weights = wh_weights$weight,
#                                                                                                             pars = wh_pars,
#                                                                                                             haz_rat = 0.3),
#                                                                                        find_weighted_hazard(the_time = yeardth,
#                                                                                                             birth_year = yearbth,
#                                                                                                             cs = bl_weights$c,
#                                                                                                             weights = bl_weights$weight,
#                                                                                                             pars = bl_pars,
#                                                                                                             haz_rat = 0.3)))
# 
# write.csv(agemort, file = "agemort.csv")


## READ AGEMORT ONCE ITS MADE

agemort <- read.csv("agemort.csv")
gg <- ggplot(agemort %>% filter(yearbth > 1915, yearbth < 1945), aes(x = age, y = rate/100000, color = race))
gg <- gg + geom_point()
gg <- gg + geom_line(aes(x = age, y = mod, color = race))
gg <- gg + geom_line(aes(x = age, y = modnotreat, color = race), linetype = "dashed")
gg <- gg + geom_line(aes(x = age, y = modcrazytreat, color = race), linetype = "dotted")
gg <- gg + facet_wrap(~yearbth)
gg <- gg + labs(colour = "Race", x = "Age", y = "Mortality Hazard", title = "Age Dependent Mortality Hazard Over Different Birth Cohorts")
print(gg)


## RESIDUALS PLOT
gg <- ggplot(agemort %>% filter(yearbth > 1915, yearbth < 1945), aes(x = age, y = mod-rate/100000, color = race))
gg <- gg + geom_point()
gg <- gg + geom_point(aes(x = age, y = modnotreat-rate/100000, color = race), shape = 38)
gg <- gg + facet_wrap(~yearbth)
gg <- gg + labs(colour = "Race", x = "Age", y = "Mortality Hazard", title = "Age Dependent Mortality Hazard Over Different Birth Cohorts")
print(gg)

