# 7.1
library(ggplot2)
library(dplyr)

mpg <- tbl_df(mpg)
mpg

# 7.2
glimpse(mpg)
head(mpg)
summary(mpg)

# 7.3
summary(mpg$hwy)
mean(mpg$hwy)
median(mpg$hwy)
range(mpg$hwy)
quantile(mpg$hwy)

opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)

# 7.3.1
hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu=mu0, alternative = "greater")
t.test(hwy) # 95% confidence range
?t.test
t.test(hwy, conf.level = .99)
t.test(hwy, conf.level = .90)

# check outlier
boxplot(mpg$hwy)

# histogram에서 40이상이 outlier임
hist(mpg$hwy, breaks=20, probability=TRUE, col=21)
lines(density(mpg$hwy), col=2, lwd=2)

# which로 해당하는 인덱스를 추출, 총 결과 길이를 출력
length(which(mpg$hwy >= 40))

# 7.3.2
c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))

# 7.4
