setwd("~/Documents/PhD/JIF/final_codes/JIF_15")

data <- read.csv("not_enough_credit_JIF_15.csv")

data$age = data$endyear - data$startyear

early <- data[data$age <= 5,]

senior <- data[data$age > 10,]

mid <- data[data$age <= 10,]
mid <- mid[mid$age > 5,]

print(nrow(early))
print(nrow(mid))
print(nrow(senior))

#t.test(early$hits,early$glam, paired = TRUE, alternative = 'greater')

#t.test(mid$hits,mid$glam, paired = TRUE, alternative = 'greater')

t.test(senior$hits, senior$glam, paired = TRUE, alternative = 'greater')