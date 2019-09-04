install.packages("gapminder")
help(package = "gapminder")
library(gapminder)
?gapminder
gapminder

head(gapminder)
tail(gapminder)

library(dplyr)
glimpse(gapminder)

gapminder$lifeExp
gapminder$gdpPercap
gapminder[, c('lifeExp', 'gdpPercap')]
gapminder[,c(4,6)]
gapminder %>% select(gdpPercap, lifeExp)

summary(gapminder$lifeExp)
summary(gapminder$gdpPercap)
cor(gapminder$lifeExp, gapminder$gdpPercap)

opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=50)
hist(sqrt(gapminder$gdpPercap), nclass=50)
hist(log10(gapminder$gdpPercap), nclass=50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(opar)

cor(gapminder$lifeExp, log10(gapminder$gdpPercap))

library(ggplot2)
library(dplyr)
library(gapminder)

gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()

library(ggplot2)
?ggplot
example(ggplot)
glimpse(df)

ggplot(gapminder, aes(lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(lifeExp)) + geom_histogram() # 데이터 셋의 변수명 자동완성지원

?diamonds
?mpg
glimpse(diamonds)
glimpse(mpg)

gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly() + scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_density() + scale_x_log10()
summary(gapminder)

diamonds %>% ggplot(aes(cut)) + geom_bar()

table(diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut)) * 100, 1)

diamonds %>% 
  group_by(cut) %>% 
  tally() %>% 
  mutate(pct=round(n/sum(n) * 100, 1))

diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha=.1)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()

pairs(diamonds %>% sample_n(1000))

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

unique(mpg$class)

mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)

mpg %>% mutate(class=reorder(class, hwy, median)) %>% 
  ggplot(aes(class, hwy)) + 
  geom_jitter(col='gray') + 
  geom_boxplot(alpha=.5)

mpg %>% 
  mutate(class=factor(class, levels=c("2seater","subcompact","compact","midsize","minivan","suv","pickup"))) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') + 
  geom_boxplot(alpha=.5)

mpg %>% 
  mutate(class=factor(class, levels=c("2seater","subcompact","compact","midsize","minivan","suv","pickup"))) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) + coord_flip()

library(dplyr)
glimpse(data.frame(Titanic))

xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))
?Titanic
Titanic

mosaicplot(Titanic, main = "Survival on the Titanic")
mosaicplot(Titanic, main = "Survival on the Titanic", color=TRUE)

apply(Titanic, c(3, 4), sum)
round(prop.table(apply(Titanic, c(3, 4), sum), margin = 1), 3)
apply(Titanic, c(2,4), sum)
round(prop.table(apply(Titanic, c(2, 4), sum), margin = 1), 3)
t2 = data.frame(Titanic)
t2 %>% group_by(Sex) %>% 
  summarize(n = sum(Freq),
            survivors=sum(ifelse(Survived=="Yes", Freq, 0))) %>% 
  mutate(rate_survival=survivors/n)
