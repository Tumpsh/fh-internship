library(dplyr)
library(ggplot2)
cr <- read.csv("mortality-crude.csv", stringsAsFactors = FALSE)
aa <- read.csv("mortality-ageadjusted.csv", stringsAsFactors = FALSE)
names(cr) <- c("race", "yod", "rate", "cnt", "pop")
names(aa) <- c("race", "yod", "rate", "cnt", "pop")
cr <- cr %>% filter(race %in% c("Black", "White"), nchar(yod)==4) 
cr <- cr %>% mutate(yod = as.numeric(yod))
aa <- aa %>% filter(race %in% c("Black", "White"), nchar(yod)==4)
aa <- aa %>% mutate(yod = as.numeric(yod))

ggplot(data = cr, aes(x=yod, y=rate, color=race)) + geom_line() + geom_point(data = aa, aes(x=yod, y=rate, color=race))

crrat <- cr %>% filter(race == "White") %>% merge(cr %>% filter(race == "Black"), by = "yod")
crrat <- crrat %>% mutate(rat = rate.y/rate.x)
aarat <- aa %>% filter(race == "White") %>% merge(aa %>% filter(race == "Black"), by = "yod")
aarat <- aarat %>% mutate(rat = rate.y/rate.x)

ggplot(data = crrat, aes(x=yod, y=rat)) + geom_line() + geom_point(data = aarat, aes(x=yod, y=rat))
