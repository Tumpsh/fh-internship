library(survival)
library(Epi)
data <- read.csv("SEERout1980_86.csv")
#data=data[data$Age_dx>=50,]
data$treat <- as.numeric(data$Tx)
data$treat <- factor(as.numeric(data$treat))
data$treat <- Relevel(data$treat, list("Other/none" = c(1, 2), "RP" = c(3), "RT"  =  c(4)))

data$stage <- as.numeric(data$Stage)
data$stage <- factor(data$stage)
data$stage <- Relevel(data$stage, list("Distant" = 1, "Local/regional" = c(2), "Unknown" = c(3)))
data$Prostatedeath = I(data$Prostate_death =="Dead")

KMout <- survfit(Surv(Intervals/12, Prostatedeath)~1 + strata(Race), 
                 data = subset(data, treat=="Other/none"&stage=="Local/regional"))


# Can see median survival for each group and use this to calculate
# the exponential parameter
bl_lambda <- -log(0.5)/18.3
wh_lambda <- -log(0.5)/21.8

old_bl_lambda <- log(2)/17.7
old_wh_lambda <- log(2)/21.7


inc_data <- read.csv("incidence-by-age.csv", stringsAsFactors = FALSE)
names(inc_data) <- c("age", "yeardx", "race", "rate", "count", "pop")
inc_data <- inc_data %>% filter(race %in% c("White", "Black"),
                                age != "Unknown",
                                nchar(yeardx) == 4)
inc_data <- inc_data %>% mutate(age = as.numeric(substr(age, 1, 2)),
                                yeardx = as.numeric(yeardx),
                                rate = rate/100000)
inc_data_bl <- inc_data %>% filter(race=="Black",
                                   yeardx %in% 1980:1986)

bl_pars <- inc_data_bl %>% group_by(age) %>% dplyr::summarise(incrate = mean(rate))
bl_pars <- bl_pars %>% mutate(survrate = bl_lambda)

inc_data_wh <- inc_data %>% filter(race=="White",
                                   yeardx %in% 1980:1986)

wh_pars <- inc_data_wh %>% group_by(age) %>% dplyr::summarise(incrate = mean(rate))
wh_pars <- wh_pars %>% mutate(survrate = wh_lambda)

#Age Distrib:
# 
# age_dist <- inc_data %>% filter(yeardx %in% 1980:1986) %>% group_by(age, race)  %>% dplyr::summarise(agesum = sum(pop))
# race_sum <- age_dist %>% group_by(race) %>% dplyr::summarise(sum(agesum))
# wh_tot <- as.numeric(race_sum[2,2])
# bl_tot <- as.numeric(race_sum[1,2])
# age_dist <- age_dist %>% mutate(total = ifelse(race=="White", wh_tot, bl_tot))
# age_dist <- age_dist %>% mutate(dens = agesum/total)
# 
# wh_age_dist <- age_dist %>% filter(race=="White")
# bl_age_dist <- age_dist %>% filter(race=="Black")
# mappings <- approx(x = wh_age_dist$age, y = 1:length(wh_age_dist$age), xout = dat$age, method = "constant", rule = 2)
# 
# wh_weightedhaz <- (wh_age_dist[mappings$y,5]/5)*dat$wh_dens/(1-(wh_age_dist[mappings$y,5]/5)*dat$wh_cdf)
# sum(wh_weightedhaz)
# bl_weightedhaz <- (bl_age_dist[mappings$y,5]/5)*dat$bl_dens/(1-(bl_age_dist[mappings$y,5]/5)*dat$bl_cdf)
# sum(bl_weightedhaz)
