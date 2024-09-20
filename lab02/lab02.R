####### Data Analytics Fall 2024 Lab 02 ######

library(ggplot2)

### set working directory
setwd("/Users/henryliu/Documents/GitHub/DataAnalytics2024_Henry_Liu/lab02/")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

### Boxplot comparing 3 variables (BDH, ECS, ECO)
boxplot(epi.results$BDH.new, epi.results$ECS.new, epi.results$ECO.new, names=c("BDH.new","ECS.new", "ECO.new"))

### Q-Q plots for 3 variables compared to some known distribution
# Q-Q Plot for BDH against a normal distribution
qqnorm(epi.results$BDH.new)
qqline(epi.results$BDH.new)

# Q-Q Plot for ECS against a normal distribution
qqnorm(epi.results$ECS.new)
qqline(epi.results$ECS.new)

# Q-Q Plot for ECO against a normal distribution
qqnorm(epi.results$ECO.new)
qqline(epi.results$ECO.new)

# Cumulative Density Functions for BDH, ECS, ECO
plot(ecdf(epi.results$ECS.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(epi.results$ECS.new))

plot(ecdf(epi.results$BDH.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(epi.results$BDH.new))

plot(ecdf(epi.results$ECO.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(epi.results$ECO.new))

# Q-Q plot BDH against the generating distribution
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),epi.results$BDH.new)
qqline(epi.results$BDH.new)

# Q-Q plot ECS against the qchisq distribution
df <- 3
x <- qchisq(ppoints(200), df = df)
qqplot(x, epi.results$ECS.new)
qqline(epi.results$ECS.new)

# Q-Q plot ECO against the qbeta distribution
shape1 <- 2
shape2 <- 5
x <- qbeta(ppoints(200), shape1 = shape1, shape2 = shape2)
qqplot(x, epi.results$ECO.new)
qqline(epi.results$ECO.new)

### ECDF plots for 3 variables compared to each other (BDH, ECS, ECO)
plot(ecdf(epi.results$BDH.new), do.points=FALSE, col="blue", main="ECDF Comparison of BDH, ECS, and ECO", xlab="Value", ylab="ECDF")
lines(ecdf(epi.results$ECS.new), col="green")
lines(ecdf(epi.results$ECO.new), col="purple")
legend("bottomright", legend=c("BDH", "BCS", "ECO"), col=c("blue", "green", "purple"), lty=1)

### Summary stats and select plots from 3 linear models
## read data
populations_2023 <- read.csv("countries_populations_2023.csv")
## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]
## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]
## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","BDH.old","BDH.new", "ECS.old", "ECS.new", "ECO.old", "ECO.new")]
## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)
## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

#created linear model of BDH.new = a(population_log) + b
lin.mod.bdhnew <- lm(BDH.new~population_log,epi.results.sub)
plot(BDH.new~population_log)
abline(lin.mod.bdhnew)
summary(lin.mod.bdhnew)
plot(lin.mod.bdhnew)

#created linear model of ECS.new = a(population_log) + b
lin.mod.ecsnew <- lm(ECS.new~population_log,epi.results.sub)
plot(ECS.new~population_log)
abline(lin.mod.ecsnew)
summary(lin.mod.ecsnew)
plot(lin.mod.ecsnew)

#created linear model of ECO.new = a(population_log) + b
lin.mod.econew <- lm(ECO.new~population_log,epi.results.sub)
plot(ECO.new~population_log)
abline(lin.mod.econew)
summary(lin.mod.econew)
plot(lin.mod.econew)
