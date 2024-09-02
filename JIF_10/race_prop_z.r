setwd("~/Documents/PhD/JIF/final_codes/JIF_10")


data <- read.csv("not_enough_credit_JIF_10_no_null_race.csv")


print(nrow(data))

hit_better <- data[data$hits > data$glam,]
print(nrow(hit_better))

glam_better <- data[data$hits < data$glam,]
print(nrow(glam_better))

###Asian
ha <- hit_better[hit_better$asi > 0.50,]
ga <- glam_better[glam_better$asi > 0.50,]

print(nrow(ha))
print(nrow(ga))

total = nrow(ha) + nrow(ga)

binom.test(x = nrow(ha), n = total, p = 0.50, alternative = 'greater')


###Black
hb <- hit_better[hit_better$black > 0.50,]
gb <- glam_better[glam_better$black > 0.50,]

print(nrow(hb))
print(nrow(gb))

total = nrow(hb) + nrow(gb)

#binom.test(x = c(nrow(hb),nrow(gb)), p = 0.50, alternative = 'greater')
binom.test(x = nrow(hb), n = total, p = 0.50, alternative = 'greater')


###White
hw <- hit_better[hit_better$white > 0.50,]
gw <- glam_better[glam_better$white > 0.50,]

print(nrow(hw))
print(nrow(gw))

total = nrow(hw) + nrow(gw)

#binom.test(x = c(nrow(hb),nrow(gb)), p = 0.50, alternative = 'greater')
binom.test(x = nrow(hw), n = total, p = 0.50, alternative = 'greater')



###Hispanic
hh <- hit_better[hit_better$his > 0.50,]
gh <- glam_better[glam_better$his > 0.50,]

print(nrow(hh))
print(nrow(gh))

total = nrow(hh) + nrow(gh)

#binom.test(x = c(nrow(hb),nrow(gb)), p = 0.50, alternative = 'greater')
binom.test(x = nrow(hh), n = total, p = 0.50, alternative = 'greater')






prop.test(x = c(5581, 925), 
          n = c(35392, 4275), 
          alternative = "less", conf.level = 0.95, correct = FALSE) 


