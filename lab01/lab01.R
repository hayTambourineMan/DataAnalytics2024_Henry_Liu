epi2024results06022024 <- read_csv("Desktop/Data Analysis/Lab/lab01/epi2024results06022024.csv")
View(epi2024results06022024)
attach(EPI_data) # sets the ‘default’ object 
EPI.new # prints out values EPI_data$EPI.new
tf <- is.na(EPI.new) # records True values if the value is NA
E <- EPI.new[!tf] # filters out NA values, new array

#Exercise 1: exploring the distribution
summary(EPI.new) # stats
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new) # stem and leaf plot
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)

boxplot(EPI.new, APO.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new) 

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 

#Exercise 2: fitting a distribution beyond histograms
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new)
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#Exercise 2a: fitting a distribution

#ECO
plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE)
qqnorm(ECO.new); qqline(ECO.new)
qqplot(rnorm(250), ECO.new, xlab = "Q-Q plot for norm dsn")
qqline(ECO.new)
qqplot(rt(250, df = 5), ECO.new, xlab = "Q-Q plot for t dsn")
qqline(ECO.new)

#BDH
plot(ecdf(BDH.new), do.points=FALSE, verticals=TRUE)
qqnorm(BDH.new); qqline(BDH.new)
qqplot(rnorm(250), BDH.new, xlab = "Q-Q plot for norm dsn")
qqline(BDH.new)
qqplot(rt(250, df = 5), BDH.new, xlab = "Q-Q plot for t dsn")
qqline(BDH.new)