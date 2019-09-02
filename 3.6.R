library(dplyr)
i2 <- tbl_df(iris)
class(i2)
i2
glimpse(i2)
iris %>% head
iris %>% head(10)
filter(gapminder, country=='Korea, Rep.')
filter(gapminder, year==2007)
filter(gapminder, country=='Korea, Rep.' & year==2007)
gapminder %>% filter(country=='Korea, Rep.')
gapminder %>% filter(year==2007)
gapminder %>% filter(country=='Korea, Rep.' & year==2007)
arrange(gapminder, year, country)
gapminder %>% arrange(year, country)

select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)

gapminder %>% 
  mutate(total_gdp = pop * gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap,
         lgrk = le_gdp_ratio * 100)

gapminder %>% 
  summarize(n_obs = n(),
            n_countries = n_distinct(country),
            n_years = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))

sample_n(gapminder, 10)
sample_frac(gapminder, 0.01)

distinct(select(gapminder, country))
distinct(select(gapminder, year))

gapminder %>% select(country) %>% distinct()
gapminder %>% select(year) %>% distinct()

gapminder %>% 
  filter(year==2007) %>% 
  group_by(continent) %>% 
  summarize(median(lifeExp))

d1 = filter(gapminder, year == 2007)
d2 = group_by(d1, continent)
d3 = summarise(d2, lifeExp = median(lifeExp))
arrange(d3, -lifeExp)

arrange(
  summarise(
    group_by(
      filter(gapminder, year==2007), continent
    ), lifeExp=median(lifeExp)
  ), -lifeExp
)

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(lifeExp = median(lifeExp)) %>% 
  arrange(-lifeExp)

(df1 <- data_frame(x = c(1,2), y =2:1))
(df2 <- data_frame(x = c(1,3), a = 10, b="a"))

df1 %>% inner_join(df2)
df1 %>% left_join(df2)
df1 %>% right_join(df2)
df1 %>% full_join(df2)

#exercise
#1-a.
gapminder %>% 
  filter(year==2007) %>% 
  group_by(country) %>% 
  summarise(mean(gdpPercap))
#1-b.
gapminder %>% 
  filter(year==2007) %>% 
  group_by(continent) %>% 
  summarise(mean = mean(lifeExp), median=median(lifeExp))


#2-a. UCI MachineLearning Repo
library(tidyverse)
setwd("/Users/manson125/Documents/workspace/SiliconValleyDataScientist/Practice/")
#Data Set https://archive.ics.uci.edu/ml/datasets/heart+Disease
# 데이터가 라인별로 구성되어 있지 않음.(여러줄에 걸쳐 하나의 데이터구성됨)
# paste 함수로 모든 라인을 연결시킨다.
x <- paste(read_lines("./UCI/switzerland.data"), collapse = " ")
# 마지막 컬럼값인 "name"으로 split해야하는데 " name "로 구성되어 있으므로
# 끝에 공백값을 추가한다. 공백을 넣으면 두개의 공백이 추가됨. 아래와같이 설정
x <- paste(x,"")
#" name "으로 split을 함. simplify = TRUE는 matrix반환,FASE는 Vector 반환
y <- str_split(x, " name ", simplify = TRUE)

#각 컬럼에 공백 구분자로 문자열 값들이 들어가 있음.
# 각 컬럼의 데이터를 다시 split하여 컬럼으로 구성해야함.
#아래는 전체 리스트를 추가할 변수 초기환
datalist = list()

#y matrix의 컬럼 만큼의 range를 구성
k <- c(1:ncol(y))
#heart-disease.names 에서 각 컬럼별 이름을 가져와 컬럼 이름을 구성
colnm <- c('id','ccf','age','sex','painloc','painexer','relrest','pncaden','cp','trestbps','htn','chol','smoke','cigs','years','fbs','dm','famhist','restecg','ekgmo','ekgday','ekgyr','dig','prop','nitr','pro','diuretic','proto','thaldur','thaltime','met','thalach','thalrest','tpeakbps','tpeakbpd','dummy','trestbpd','exang','xhypo','oldpeak','slope','rldv5','rldv5e','ca','restckm','exerckm','restef','restwm','exeref','exerwm','thal','thalsev','thalpul','earlobe','cmo','cday','cyr','num','lmt','ladprox','laddist','diag','cxmain','ramus','om1','om2','rcaprox','rcadist','lvx1','lvx2','lvx3','lvx4','lvf','cathef','junk')

for (i in k) {
  # ... make some data
  #transpose를 사용해 str_split결과로 만들어진 행렬을 벡터로 치환, 각 값은 factor FALSE로 만든다. as.data.frame함수에서 col.names 속성값은 동작하지 않음.
  dat <- as.data.frame(transpose(str_split(y[[i]], " ")), stringsAsFactors=FALSE, col.names = colnm)
  #컬럼명을 바꿔준다.
  colnames(dat) <- colnm
  #dat$i <- i  # maybe you want to keep track of which iteration produced it?
  #data.frame으로 변환된 데이터를 리스트에 추가
  datalist[[i]] <- dat # add it to your list
}
# 마지막 값에 쓰레기 값이 들어가 있으므로 삭제
datalist[[ncol(y)]] <- NULL
# rbind함수로 각 리스트를 행별로 취합하면 완성!!
big_data = do.call('rbind', datalist)


#2-b. R example
#https://vincentarelbundock.github.io/Rdatasets/
  
#2-c. Kaggle
# https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
  
#2-d. Wikipidia
#https://en.wikipedia.org/wiki/List_of_datasets_for_machine-learning_research
