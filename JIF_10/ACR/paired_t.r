setwd("~/Documents/PhD/JIF/final_codes/JIF_10/ACR")

data <- read.csv("not_enough_credit_merged_acr_10.csv")

data$age = data$endyear - data$startyear

early <- data[data$age <= 5,]

senior <- data[data$age > 10,]

mid <- data[data$age <= 10,]
mid <- mid[mid$age > 5,]

print(nrow(early))
print(nrow(mid))
print(nrow(senior))

#t.test(early$hits,early$glam, paired = TRUE, alternative = 'greater')

t.test(mid$hits_acr,mid$glam, paired = TRUE, alternative = 'greater')

#t.test(senior$hits_acr, senior$glam, paired = TRUE, alternative = 'greater')