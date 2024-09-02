setwd("~/Documents/PhD/JIF/final_codes/JIF_10/ACR")

data <- read.csv("not_enough_credit_merged_acr_10_no_null_race.csv")
print(head(data))

print(nrow(data))

hit_better <- data[data$hits_acr > data$glam,]
print(nrow(hit_better))

glam_better <- data[data$hits_acr < data$glam,]
print(nrow(glam_better))

print(head(hit_better))

print(head(glam_better))

##approach 3: main version
x1 <- hit_better$black
y1 <- glam_better$black

x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

#x2 <- sample(x=x2, size=length(y2))
#x2 <- sort(x2, decreasing = T)

x3 <- cumsum(x2)
y3 <- cumsum(y2)

ind <- sample(1:length(x3), length(y3), replace = FALSE)
#ind2 <- sort(ind, decreasing = F) 
x4 <- x3[ind]

#print(length(x4))
x5 <- sort(x4, decreasing = F)


print(length(x5))
print(length(y3))

print(x5)
print(y3)

res = ks.test(x = x5, y = y3, alternative = "two.sided") #
print(res)
print(mean(x5))
print(mean(y3))

plot(x5, type ="l", col = "green", ylab="Cumulative sum of racial probability",main = "Black")
lines(y3, col = "red", lty = 2)
legend(2000, 2000, legend=c("hit_better", "glam_better"),
       col=c("green", "red"), lty=1:2, cex=0.8)



dev.off()
#take cumulative sum, than select random indices from hits, pick the values and sort ascending. Finally, run
#k-s test on the data 


##Do the test on some simulated data
##manwhitney u test - cumsum















var <- "asi"

##approach 1: take a random, same size sample from hit_better, no lnorm.
hit_sample = hit_better[sample(nrow(hit_better), size=nrow(glam_better)), ]
print(nrow(hit_sample))
x1 <- hit_sample$asi
y1 <- glam_better$asi
x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

x3 <- cumsum(x2)
y3 <- cumsum(y2)
res = ks.test(x3,y3,alternative = "less")
print(res)

plot(x3, type ="l", col = "green", ylim = c(0,500)) #, ylim = c(0,500)
lines(y3, col = "red")

##approach 2: i think same as approach 1.
x1 <- hit_better$asi
y1 <- glam_better$asi

x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

x2 <- sample(x=x2, size=length(y2))
x2 <- sort(x2, decreasing = T)

x3 <- cumsum(x2)
y3 <- cumsum(y2)
print(length(x3))
print(length(y3))


res = ks.test(x3,y3)
print(res)

plot(x3, type ="l", col = "green",ylim=c(0,500))
lines(y3, col = "red")





##approach 4:

y1 <- glam_better$white
y1 <- dlnorm(y1)

x1 <- hit_better$white
x1 <- dlnorm(x1)
x1 <- sample(x1, size = length(y1))

x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

y3 <- cumsum(y2)
x3 <- cumsum(x2)

res = ks.test(x3,y3)
print(res)

plot(x3, type ="l", col = "green")
lines(y3, col = "red")

##approach 5:

y1 <- glam_better$white
y1 <- plnorm(y1)
print(y1)

x1 <- hit_better$white
x1 <- sample(x1, size = length(y1))
x1 <- plnorm(x1)

x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

y3 <- cumsum(y2)
x3 <- cumsum(x2)

res = ks.test(x3,y3)
print(res)

plot(x3, type ="l", col = "green")#ylim = c(0, 250)
lines(y3, col = "red")
#plot(x3, type='l')\

#test
y1 <- glam_better$white
y1 <- plnorm(y1)
print(y1)

x1 <- hit_better$white
x1 <- sample(x1, size = length(y1))
x1 <- plnorm(x1)

x2 <- sort(x1, decreasing = T)
y2 <- sort(y1, decreasing = T)

y3 <- cumsum(y2)
x3 <- cumsum(x2)

res = ks.test(x3,y3)
print(res)

plot(x3, type ="l", col = "green")#ylim = c(0, 250)
lines(y3, col = "red")


y1 <- seq(0, 10)

plnorm(y1)

v <- c(1, 2, 3, 4, 8,7,5,6)
s <- sample(1:length(v), 3, replace = FALSE) #sample(x = v, size = 3)
print(s)
print(v[s])
print(sort(v[s], decreasing = T))



