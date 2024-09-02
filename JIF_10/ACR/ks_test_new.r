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
x1 <- hit_better$white
y1 <- glam_better$white

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

#print(x5)

res = ks.test(x = x5, y = y3, alternative = "l") #
print(res)
print(mean(x5))
print(mean(y3))

plot(x5, type ="l", col = "green", ylab="Cumulative sum of racial probability",main = "Black")
lines(y3, col = "red", lty = 2)
legend(2000, 2000, legend=c("hit_better", "glam_better"),
       col=c("green", "red"), lty=1:2, cex=0.8)



dev.off()



a1 <- hit_better$white
b1 <- glam_better$white

if (any(is.na(b1))) {
  print("Warning: NA values detected in b1.")
}

a2 <- sample(a1, size=length(b1), replace = FALSE)
#print(a2)

res = ks.test(x = a2, y = b1, alternative = "l") #
print(res)
print(mean(a2))
print(mean(b1))

print(length(a2))
print(length(b1))


