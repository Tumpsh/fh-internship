stdpop <- read.csv("2000-Standard-Pop.csv", stringsAsFactors = FALSE)
stdpop <- stdpop %>% mutate(low = as.numeric(substr(age, 1, 2)),
                            high = as.numeric(substr(age, 4, 5)),
                            rate = pop/1000000)
stdpop[1, 4] <- 0
stdpop[19, 1] <- "85-99 years"
stdpop[19, 4] <- 99
stdpop <- stdpop %>% rbind(data.frame(age = "100+ years", pop = 0, low = 100, high = 100000, rate = 0))
