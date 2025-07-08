#Extract data from Mousseau
#

rm(list = ls())
setwd("C:/Users/mcu08001/Documents/1New Research/H23/Data")
# data <- metaDigitise(dir = root.dir)
# metaDigitise()
# library(metaDigitise)
# data <- metaDigitise(dir = root.dir, summary = FALSE)
# 
# save(data,file= "Mousseau fig.RDS")

load("Mousseau fig.RDS")

# Life history data
test.data <- data[data$id == "l,b,p,m",]
data.len <- nrow(test.data)
N <- 341
test.data$abs.N <- round(test.data$y * N,0)
herit <- rep(NA,N)
j=1
for (i in 2:data.len){
  temp<- rep(test.data$x[i],((test.data$abs.N[i] - test.data$abs.N[i-1])))
  L <- length(temp)
  if(L > 0) {herit[j:(j+L-1)] <- temp
    j = j +L
    }
}
hist(herit)
cat("Mean heritability = ", round(mean(herit),3), "study mean = .262")
cat("SE heritability = ", round(sd(herit)/sqrt(N),3), "study SE = .012")
cat("Median heritability = ", round(median(herit),2), "study median = .25")
LHT.herit <- herit
herit.all <- herit

# Physiology data
test.data <- data[data$id == "p",]
data.len <- nrow(test.data)
N <- 104
test.data$abs.N <- round(test.data$y * N,0)
herit <- rep(NA,N)
test.data$x = test.data$x-0.033 #slighlty correct to match real data mean 0.033/.330 = 0.10
j=1
for (i in 2:data.len){
  temp<- rep(test.data$x[i],((test.data$abs.N[i] - test.data$abs.N[i-1])))
  L <- length(temp)
  if(L > 0) {herit[j:(j+L-1)] <- temp
  j = j +L
  }
}
hist(herit)
cat("Mean heritability = ", round(mean(herit),3), "study mean = .330")
cat("SE heritability = ", round(sd(herit)/sqrt(N),3), "study SE = .027")
cat("Median heritability = ", round(median(herit),3), "study median = .262")
herit.all<-c(herit.all,herit)

# Behavior data
test.data <- data[data$id == "b",]
data.len <- nrow(test.data)
N <- 105
test.data$abs.N <- round(test.data$y * N,0)
herit <- rep(NA,N)
test.data$x = test.data$x+0.0 #no correction needed
j=1
for (i in 2:data.len){
  temp<- rep(test.data$x[i],((test.data$abs.N[i] - test.data$abs.N[i-1])))
  L <- length(temp)
  if(L > 0) {herit[j:(j+L-1)] <- temp
  j = j +L
  }
}
hist(herit)
cat("Mean heritability = ", round(mean(herit),3), "study mean = .302")
cat("SE heritability = ", round(sd(herit)/sqrt(N),3), "study SE = .023")
cat("Median heritability = ", round(median(herit),3), "study median = .280")
herit.all<-c(herit.all,herit)

# Morph data
test.data <- data[data$id == "m",]
data.len <- nrow(test.data)
N <- 570
test.data$abs.N <- round(test.data$y * N,0)
test.data$abs.N[14] = 80 # fix point that is less than previous
herit <- rep(NA,N)
test.data$x = test.data$x-0.019 #slightly correct to match real data mean 0.019/.461 = 0.041
j=1
for (i in 2:data.len){
  #print(i)
  temp<- rep(test.data$x[i],((test.data$abs.N[i] - test.data$abs.N[i-1])))
  L <- length(temp)
  if(L > 0) {herit[j:(j+L-1)] <- temp
  j = j +L
  }
}
herit[N] = herit[N-1] # missing one, so add one to last

hist(herit)
cat("Mean heritability = ", round(mean(herit),3), "study mean = .461")
cat("SE heritability = ", round(sd(herit)/sqrt(N),3), "study SE = .004")
cat("Median heritability = ", round(median(herit),3), "study median = .428")
herit.all<-c(herit.all,herit)

save(herit.all,file = "Mousseau herit.rds")
save(LHT.herit,file = "Mousseau LHT herit.rds")
write.csv(LHT.herit, file = "Mousseau LHT herit.csv")
