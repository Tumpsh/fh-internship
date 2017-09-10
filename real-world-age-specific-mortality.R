## Old method which didn't work

# agemort <- read.csv("age_specific_mortality.csv")
# agemort <- agemort[2:19,c(1, seq(5, 141, 3))]
# names(agemort) <- c("age", 1969:2014)
# agemort <- agemort[rep(row.names(agemort), 5), ]
# rownames(agemort) <- as.numeric(rownames(agemort))
# agemort <- agemort[order(row.names(agemort)), ]
# agemat <- as.matrix(agemort[row.names(agemort) > 10, ], dimnames = agemort$age)
# plot(c(rep(0,40),as.numeric(diag(agemat[,-1]))/100000))
# 
# nedat <- data.frame(age = character(), rate = character())
# for(i in 1969:2014){
#   nedat <- agemort[, c(age, as.character(i))] %>% rbind(nedat)
# }
# 

## Reading in csv with rates per 100000 of mortality, stratified by race,
## year of death and age at death. By manipulating data into year of birth,
## we can approximate mortality of birth cohorts along the years. For 
## instance, the people who die in year 1990, who are 60 when they die 
## would be considered in the same cohort as people who die in 1995, who are 
## 65 when they die. 
##
## In this case, we are given 5-year age groups for age of death, and we are
## assuming each person was exactly at the middle of the age group when they
## died.

agemort <- read.csv("long-age-specific-mortality.csv", stringsAsFactors = FALSE)
names(agemort) <- c("race", "age", "yeardth", "rate", "count", "pop")

agemort <- agemort %>% filter(race %in% c("White", "Black"))
agemort <- agemort %>% filter(nchar(yeardth) == 4,
                              age != "Unknown") 
agemort <- agemort %>% mutate(age = as.numeric(substr(age, 1, 2)) + 2.5,
                              yearbth = as.numeric(yeardth) - age,
                              yeardth = as.numeric(yeardth))
agemort[is.na(agemort)] <- 0
## Looking at people with just one birth year

 # gg <- ggplot(data = agemort %>% filter(yearbth == 1915.5), aes(x = age, y = rate, color = race)) + geom_point()
 # print(gg)

 
