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
