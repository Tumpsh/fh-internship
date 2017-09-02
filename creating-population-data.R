suppressWarnings(source("2000-std-pop.R"))
agemort <- read.csv("agemort.csv")

agemort <- agemort %>% mutate(low = age - 2.5)
agemort <- agemort %>% merge(stdpop, by = "low")
agemort <- agemort %>% mutate(wmod = mod*rate.y*100000,
                              wmodnotreat = modnotreat*rate.y*100000,
                              wmodcrazytreat = modcrazytreat*rate.y*100000,
                              wrate = rate.x*rate.y)
popmort <- ddply(agemort, c("race", "yeardth"), 
                 dplyr::summarise, 
                 mod = sum(wmod),
                 modnotreat = sum(wmodnotreat),
                 modcrazytreat = sum(wmodcrazytreat),
                 observed = sum(wrate))
gg <- ggplot(data = popmort) 
gg <- gg + geom_line(aes(x = yeardth, y = mod, color = race))
gg <- gg + geom_line(aes(x = yeardth, y = modnotreat, color = race), linetype = "dashed")
gg <- gg + geom_line(aes(x = yeardth, y = modcrazytreat, color = race), linetype = "dashed")
gg <- gg + geom_point(aes(x = yeardth, y = observed, color = race))
print(gg)


popdisp <- merge(popmort[popmort$race == "Black",], popmort[popmort$race == "White",], by = "yeardth")
popdisp <- popdisp %>% mutate(riskratmod = mod.x/mod.y,
                              riskratmodnotreat = modnotreat.x/modnotreat.y,
                              riskratmodcrazytreat = modcrazytreat.x/modcrazytreat.y,
                              riskratobserved = observed.x/observed.y,
                              riskdiffmod = mod.x-mod.y,
                              riskdiffmodnotreat = modnotreat.x-modnotreat.y,
                              riskdiffmodcrazytreat = modcrazytreat.x-modcrazytreat.y,
                              riskdiffobserved = observed.x-observed.y)

gg <- ggplot(data = popdisp)
gg <- gg + geom_line(aes(x = yeardth, y = riskratmod), color = "red")
gg <- gg + geom_line(aes(x = yeardth, y = riskratmodnotreat), linetype = "dashed", color = "blue")
gg <- gg + geom_line(aes(x = yeardth, y = riskratmodcrazytreat), linetype = "dashed", color = "green")
gg <- gg + geom_line(aes(x = yeardth, y = riskratobserved))
print(gg)

gg <- ggplot(data = popdisp)
gg <- gg + geom_line(aes(x = yeardth, y = riskdiffmod), color = "red")
gg <- gg + geom_line(aes(x = yeardth, y = riskdiffmodnotreat), linetype = "dashed", color = "blue")
gg <- gg + geom_line(aes(x = yeardth, y = riskdiffmodcrazytreat), linetype = "dashed", color = "green")
gg <- gg + geom_line(aes(x = yeardth, y = riskdiffobserved))
print(gg)
