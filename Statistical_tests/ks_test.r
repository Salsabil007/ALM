setwd("~/Documents/PhD/JIF/final_codes/JIF_15")

data <- read.csv("not_enough_credit_JIF_15_no_null_race.csv")
print(head(data))

print(nrow(data))

hit_better <- data[data$hits > data$glam,]
print(nrow(hit_better))

glam_better <- data[data$hits < data$glam,]
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

#print(x5)

res = ks.test(x5,y3, alternative = 'two.sided')
print(res)

#png("r_hispanic.png", width = 500, height = 400)


plot(x5, type ="l", col = "green", ylab="Cumulative sum of racial probability",lwd=5.0, main = "Hispanic",
     cex.lab = 1.5, cex.axis = 1.5)
lines(y3, col = "red", lty = 2, lwd=5.0)

legend(800, 2000, legend=c("recognized by article citation", "recognized by journal"),
       col=c("green", "red"), lty=1:2, cex=1.3)#hispanic



#legend(800, 2000, legend=c("recognized by article citation", "recognized by journal"),
#       col=c("green", "red"), lty=1:2,lwd=c(5.0,5.0), cex=1.3) #black



#legend(0, 22000, legend=c("recognized by article citation", "recognized by journal"),
#       col=c("green", "red"), lty=1:2, cex=1.3) #white

#legend(800, 4200, legend=c("recognized by article citation", "recognized by journal"),
#       col=c("green", "red"), lty=1:2, cex=1.3) #asian




dev.off()


