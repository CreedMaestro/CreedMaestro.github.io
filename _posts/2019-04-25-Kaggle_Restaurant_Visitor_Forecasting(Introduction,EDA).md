---
title: "캐글 Restaurant Visitor Forecasting Introduction, EDA with R (한글 번역)"
author: "Maestro"
date: "2019/04/25"
categories: Kaggle
tags: Kaggle, 캐글, 캐글코리아, 고객예측
layout: post
output: 
  md_document:
    variant: markdown_github
---


<center><img src="https://media.timeout.com/images/102582609/image.jpg"></center>


# 1 Introduction

Competition: Recruit Restaurant Visitor Forecasting
목표: 미래의 식당 고객 예측
Time-series 분석이며, 일본 2개 웹사이트(hpg & AirREGI)에서 나온 8개의 연관 파일로 이뤄진 일식당의 데이터다. 
데이터의 용량이 작고, 쉬워서 초보자가 접근하기에 쉬운 Competition이다.
Train set은 2016년 1월 ~ 2017년 4월이며, Test set은 2017년 4월 마지막 주 ~ 5월까지 포함한다.
Test set은 일본에서 'Golden Week'라고 불리는 기간이다. 이 때는 폐점한 식당과 고객이 없는 날들이 있어 이 구간은 점수 산정에서 제외한다. Train set 역시 식당이 폐점한 날은 제외한다.
현재 작업중인 파일이라 일단락이 된 후에 캐글에 업로드 예정이다.
파일 몇 개를 조합하여 커널을 만들 예정이며, 조합 전 파일들은 참조로 달아두겠다.

[heads or tails 원문 링크, EDA]("https://www.kaggle.com/headsortails/be-my-guest-recruit-restaurant-eda/report")
[캐글 번역 링크, Restaurant_visitor_Forecasting]("https://www.kaggle.com/maestroyi/restaurant-visitor-forecasting-eda-with-r?scriptVersionId=14464395")

각 파일의 간략한 설명:

- **air_visit_data.csv**: air restaurants의 과거 방문 data. main training data set이다.

- **air_reserve.csv** / **hpg_reserve.csv**: *air* & *hpg* 레스토랑 예약 시스템

- **air_store_info.csv** / **hpg_store_info.csv**: *air* & *hpg* 레스토랑 세부 정보

- **store_id_relation.csv**: *air* & *hpg* id

- **date_info.csv**: 일본 공휴일 정보

- **sample_submission.csv**: *test* set으로 사용.  'id'는 *air* id와 방문 날짜를 결합하여 생성


# 2 Preparations {.tabset .tabset-fade .tabset-pills}

## 2.1 Load libraries

```{r}
# general visualisation
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(corrplot)

# general data manipulation
library(dplyr)   # data manipulation
library(readr)   # input/output
library(data.table) #data manipulation
library(tibble)  # data wrangling
library(tidyr)   # data wrangling
library(stringr) # string manipulation
library(forcats) # factor manipulation

# specific visualisation
library(ggrepel)
library(ggridges)
library(ggExtra)
library(ggforce)
library(viridis)

# speific data manipulation
library(lazyeval) # data wrangling
library(broom)    # data wrangling
library(purrr)    # string manipulation

# Date plus forecast
library(lubridate) # date and time
library(timeDate)  # date and time
library(tseries)   # time series analysis
library(forecast)  # time series analysis
library(prophet)   # time series analysis
library(timetk)    # time series analysis


# Maps / geospatial
library(geosphere)  # geospatial locations
library(leaflet)    # maps
library(leaflet.extras) # maps
library(maps)  # maps
```

## 2.2 Helper functions
*multiplot* function 생성, [R Cookbooks](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/) 링크이며, multi-panel plot을 만드는 기능이다. 

```{r}
# multiplot 간략 설명

# ggplot 차트를 한 화면에 여러 개 표현하기 위함이다.
# - cols: 레이아웃 컬럼의 수이다
# - layout: 레이아웃 행렬 배치이다. 'cols'가 있으면 무시된다.
#
# ex(matrix(c(1,2,3,3,), nrow = 2, byrow= TRUE))이면 1번 차트는 좌상향, 2번 차트는 우상향, 3번 차트는 아래쪽에 배치된다.

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  
  # arguments and plotlist로 부터 list 생성
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # 만약 layout이 NULL이면, 'cols'를 사용한다.
  if (is.null(layout)) {
    # Make the panel
    # ncol: plot의 컬럼 수
    # nrow: 열의 수로 계산된 필요 행의 수
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # 정해진 위치에 차트 생성
    for (i in 1:numPlots) {
      # 이 subplot을 포함하는 영역의 i, j 행렬 위치 얻기
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}
```

```{r}
# 이항 신뢰 수준(binomial confidence levels) 추출 함수
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c('lwr','upr')))
```

## 2.3 Load data

data.table 패키지의 fread 함수로 빠른 data load

```{r}
on_kaggle <- 1

if (on_kaggle == 0) {
  rpath <- ''
  wpath <- ''
  wdpath <- '1-1-16_5-31-17_Weather/'
} else {
  rpath <- '../input/recruit-restaurant-visitor-forecasting/'
  wpath <- '../input/rrv-weather-data/'
  wdpath <- '../input/rrv-weather-data/1-1-16_5-31-17_Weather/1-1-16_5-31-17_Weather/'
}

```

```{r}
air_visits <- as.tibble(fread(str_c(rpath,'air_visit_data.csv')))
air_reserve <- as.tibble(fread(str_c(rpath,'air_reserve.csv')))
hpg_reserve <- as.tibble(fread(str_c(rpath,'hpg_reserve.csv')))
air_store <- as.tibble(fread(str_c(rpath,'air_store_info.csv')))
hpg_store <- as.tibble(fread(str_c(rpath,'hpg_store_info.csv')))
holidays <- as.tibble(fread(str_c(rpath,'date_info.csv')))
store_ids <- as.tibble(fread(str_c(rpath,'store_id_relation.csv')))
test <- as.tibble(fread(str_c(rpath,'sample_submission.csv')))
```

# 3 Overview: File Structure and content {.tabset .tabset-fade .tabset-pills}

'summary'와 'glimpse'로 개략적인 data 확인

## 3.1 Air visits

```{r}
summary(air_visits)
```


```{r}
glimpse(air_visits)
```

 *visit\_date* and *air\_store\_id*의 각각의 *visitors* 수를 포함하는 파일이다. 'date' feature는 time-series format으로 형변환 해야 한다. 
 
```{r}
air_visits %>% distinct(air_store_id) %>% nrow()
```
 
 829개의 다른 가게가 있는 걸로 보아 data set의 크기가 작다.
 
## 3.2 Air Reserve
 
```{r}
summary(air_reserve)
```
 
```{r}
glimpse(air_reserve)
```

'air reservation' 변수는 air_store 가게들의 방문 시간과 기일이 포함되어 있다.

```{r}
air_reserve %>% distinct(air_store_id) %>% nrow()
```

이 데이터는 air store 314곳의 예약 정보가 있다.

## 3.3 Hpg reserve

```{r}
summary(hpg_reserve)
```


```{r}
glimpse(hpg_reserve)
```

'hpg reservation' 변수는 hpg_store 가게들의 방문 시간과 기일이 포함되어 있다. air 파일과 동일한 구조이다.

```{r}
hpg_reserve %>% distinct(hpg_store_id) %>% nrow()
```

이 데이터는 hpg store 13325곳의 예약 정보가 있다.

## 3.4 Air Store

```{r}
summary(air_store)
```
```{r}
glimpse(air_store)
```

'air_store' 변수는 가게 지명과 위치가 포함된다. 'air_area_name' 폰트가 깨지는데 한글엔 없는 표기라서 그렇다.
'Hyōgo-ken Kōbe-shi Kumoidōri' 처음 value인데 보면 o위에 -가 들어간 문자가 '흲'으로 표기가 된다.


## 3.5 Hpg Store
```{r}
summary(hpg_store)
```

```{r}
glimpse(hpg_store)
```

'hpg_store'역시 가게 지명과 위치가 포함되며, 'air_store'와 같이 o 문자 폰트가 깨진다.
'genre_name' feature에는 word style이 명시되어 있는데 이건 'air data'와 동일한지, 아니면 'japanese style'인지 구분하는 지표다. 4690개의 다른 'hpg_store_id'가 있는데, 1.3만개가 넘었던 예약 데이터의 수보다 훨씬 적다.

## 3.6 Holidays

```{r}
summary(holidays)
```

```{r}
glimpse(holidays)
```

'date_info' file을 로드할 때 'holidays'로 명명해서 열었는데, 이건 이 데이터 안에 'holiday'가 포함되어 있기 때문이다.
'Holidays'는 이항으로 구분되어 인코딩된 integer 형식이다. 후에 data를 살펴볼려면 integer가 아닌 논리적 형식이 되어야 한다.

## 3.7 Store IDs

```{r}
summary(store_ids)
```

```{r}
glimpse(store_ids)
```

이건 'air' & 'hpg'와 연결하는 관계형 파일이다. 150쌍이 있고, 이건 모든 air store의 20% 보다 적다.

## 3.8 Test data

```{r}
summary(test)
```

```{r}
glimpse(test)
```

'air_id'와 'date'가 결합된 최종 제출 파일이다.

## 3.9 Missing values

```{r}
sum(is.na(air_visits))
sum(is.na(air_reserve))
sum(is.na(hpg_reserve))
sum(is.na(air_store))
sum(is.na(hpg_store))
sum(is.na(holidays))
sum(is.na(store_ids))
sum(is.na(test))
```

깔끔하게 결측치가 없는 data이다.

## 3.10 Reformatting features
date/time features의 형식을 바꾸고, 몇 몇 feature는 탐색하기 쉽게 logical/factor형으로 변환하겠다.
```{r}
air_visits <- air_visits %>% mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>% mutate(visit_datetime = ymd_hms(visit_datetime), 
                                      reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>% mutate(visit_datetime = ymd_hms(visit_datetime),
                                      reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>% mutate(air_genre_name = as.factor(air_genre_name),
                                  air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>% mutate(hpg_genre_name = as.factor(hpg_genre_name),
                                  hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>% mutate(holiday_flg = as.logical(holiday_flg), date = ymd(calendar_date))
```

# 4Individual feature visualisation

세부 분석을 위한 결합 전에 개별 data file의 feature 분포를 한 번 보겠다.

## 4.1 Air Visits

'air restaurant'의 full training time기준 일별 고객의 총 수와 일별/월별 고객의 중위값을 확인하겠다.
```{r}
p1 <- air_visits %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line(col = 'blue') +
  labs(y = '총 고객', x = 'Date')
  
p2 <- air_visits %>% 
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = 'orange') +
  geom_histogram(fill = 'blue', bins = 30) +
  scale_x_log10() +
  labs(x = '일 방문 고객수')

p3 <- air_visits %>%
  mutate(wday = lubridate::wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = '요일별', y = '고객 중위값')

p4 <- air_visits %>% 
  mutate(month = lubridate::month(visit_date, label = TRUE)) %>% 
  group_by(month) %>% 
  summarise(visits = median(visitors)) %>% 
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = 'none') +
  labs(x = '월별', y = '고객 중위값')

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow = TRUE)
multiplot(p1, p2, p3, p4, layout = layout)
```

결과:

- Time-series 상에서 흥미로운 장기 흐름 구조가 있는 것 같다. 이건 신규 레스토랑이 데이터 베이스에 추가된 것과 연관이 있어 보인다. 게다가, 주별 흐름에 연동된 기간 패턴을 볼 수 있었다.


- 2번째 차트는 매장별 일 방문 고객수가 20명 근처(오랜지색 선)인 곳이 가장 많은 것으로 보인다. 드문 경우이지만 일 방문 고객 수가 100명이 넘는 곳도 간혹 있다.

- 예상대로 금요일과 주말이 가장 인기있는 날로 보인다. 월.화는 방문 고객수의 중위값이 가장 낮다.

- 연 기준 차트를 보면 특정 변동이 보인다. 12월이 가장 인기있는 달로 보이고, 3 ~ 5월은 꾸준하게 바빠보인다.

2017년 4월 마지막 주 ~ 5월까지의 예측을 위해 2016년 training data를 살펴보겠다.

```{r}
air_visits %>% 
  filter(visit_date > ymd('2016-04-15') & visit_date < ymd('2016-06-15')) %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() + 
  geom_smooth(method = 'loess', color = 'blue', span = 1/7) +
  labs(y = '총 고객', x = 'Date')
```

검은 선은 날짜이며, 파란 선은 회색 신뢰 영역에 해당하는 smoothing fit이다.
여기서 2016년 4월 29일 ~ 5월 5일까지의 골든 위크의 영향을 다시 볼 수 있다.

## 4.2 Air Reservations

예약 data와 실제 방문한 고객의 수를 비교해보자. 방문 시간대 차트와 예약 후 방문까지의 시간 차트를 나란히 놓고 시각화해보겠다.
```{r}
foo <- air_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = lubridate::wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = lubridate::wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
         )

p1 <- foo %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() + 
  labs(x = "'air' visit date")

p2 <- foo %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = 'blue') +
  labs(x = '방문 시간')

p3 <- foo %>% 
  filter(diff_hour < 24*5) %>% 
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(diff_hour, all_visitors)) + 
  geom_col(fill = 'blue') +
  labs(x = '예약 후 방문까지의 시간')

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout = layout)
```

결과:

- 2016년의 예약은 많이 저조해보이고, 심지어 긴시간 없는 구간도 보인다. 16년 연말 동안만 예약이 늘었다.
2017년의 고객수는 강하게 머물러 있고, 1분기 이후의 인위적인 감소는 training time이 끝나는 시기의 이런 예약과 연관되어 보인다. 이건 장기 예약이 data set에 미포함된걸 의미한다.

- 일반적으로 저녁 식사를 하기 위해 직전에 예약이 이루어 진다.

- 예약 후 방문까지의 시간은 24시간 주기를 보인다. 방문 몇 시간 전 예약하는 경우가 가장 많지만, 예약이 불가능하면 가능한 다른 날을 예약하는걸로 보인다. 이 차트는 더 긴 시간의 패턴을 보이기 위해 끝이 잘린다. 예약 후 방문까지의 시간이 긴 것은 예외적인게 아니다. 가장 긴 시간은 1년이 넘는 시간이 있다. 

```{r}
foo %>% 
  arrange(desc(diff_day)) %>% 
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>% 
  head(5)
```

가장 긴 시간을 정렬해보면 top5에 단지 2식당만이 있다. 이건 정말로 핫플레이스거나 아니면 데이터 입력의 오류일 것이다.

## 4.3 HPG Reservation

위와 동일하게 예약 data와 실제 방문한 고객의 수를 비교해보겠다.
```{r}
foo <- hpg_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
         )

p1 <- foo %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'hpg' visit date")

p2 <- foo %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = 'red') +
  labs(x = '방문 시간')

p3 <- foo %>% 
  filter(diff_hour < 24*5) %>% 
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = 'red') +
  labs(x = '예약 후 방문까지의 시간')

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout=layout)

```

결과:

- 예약 방문 고객은 2016년 12월에 많아지는 패턴을 보이며, 위의 'air' data와 같이 time frame의 끄트머리는 수치가 떨어지는걸 볼 수 있다.

- 방문 시간은 위와 동일하게 저녁대가 가장 많지만, 예약 후 방문까지의 시간은 또 다른 24시간 패턴을 볼 수 있다.
몇 시간 전의 예약이 하루나 이틀전의 예약보다 특출나게 많지가 않다. 이건 'air' data와는 완연한 대조를 보여준다.


## 4.4 Air Store

시각적인 측면을 시각화하고, 공간적인 측면을 보겠다.
data 설명에 따르면 'latitude'와 'longitude'는 식당이 있는 지역의 '위도'와 '경도'이다. 이는 상점의 위치를 특정하지 못하게 한다. 

이 지도는 air restaurants의 인터랙티브 맵이고, 클러스터를 클릭하면 지도가 확대되며 마지막엔 식당 분류 타입으로 나뉜다.
지도는 많은 식당이 같은 좌표를 공유한다는 걸 보여주고, single marker를 클릭하면 식당의 id가 나온다.

```{r}
# 지도 mapping
leaflet(air_store) %>% 
  addTiles() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addMarkers(~longitude, ~latitude, popup= ~air_store_id, label = ~air_genre_name,
             clusterOptions = markerClusterOptions())
```

여러 식당 타입 분포와 가장 많은 레스토랑이 있는 곳을 차트로 나타내겠다.

```{r}
p1 <- air_store %>% 
  group_by(air_genre_name) %>% 
  count() %>% 
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name)) +
  geom_col() +
  coord_flip() + 
  theme(legend.position = 'none') +
  labs(x = "'air_genre_name'분류 타입", y = 'air restaurants의 수')

p2 <- air_store %>% 
  group_by(air_area_name) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(15, n) %>% 
  ggplot(aes(reorder(air_area_name, n, FUN = min), n, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = 'none') +
  coord_flip() +
  labs(x = "'air_area_name' Top15 지역", y = 'air restaurants의 수')

layout <- matrix(c(1,2),2,1, byrow = TRUE)
multiplot(p1, p2, layout=layout)
```

결과:

- 이자카야 타입이 가장 많고, 다음이 카페이다. 'Karaoke'와 'International', 'Asian' 타입 식당은 적어 보인다.

- 후쿠오카는 면적당 air restaurants가 가장 많다. 그 뒤를 이어 Tokyo 지역이다.

## 4.5 HPG Store

위의 air store와 같이 인터랙티브 지도를 만들겠다.
```{r}
leaflet(hpg_store) %>% 
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(~longitude, ~latitude, popup = ~hpg_store_id, label = ~hpg_genre_name,
             clusterOptions = markerClusterOptions())

```

hpg restaurants의 지역과 genre를 나타내겠다.

```{r}
p1 <- hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "'hpg_genre_name'분류 타입", y = 'hpg restaurants의 수')

p2 <- hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 20)) %>%
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(15, n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "'hpg_area_name' Top 15 지역", y = 'hpg restaurants의 수')

layout <- matrix(c(1,2),1,2,byrow= TRUE)
multiplot(p1, p2, layout = layout)
```

결과:

- hpg 설명에는 'air' data보다 많은 genre 분류가 있다. 'air' data에선 세분화된 많은 분류들이 여기에선 'Japanese Style'에 포함된 것 처럼 보인다. 'International cuisine.'은 동일하게 ㅇㅆ다.

- Top 15에는 도쿄와 오사카가 많이 들어 있음을 알 수 있다.

## 4.6 Holidays

2017년의 예측 기간 data와 2016년의 동일 기간 data의 분포를 나타내는 차트와 두 해를 합한 차트를 보자.

```{r}
foo <- holidays %>% 
  mutate(wday = wday(date))

p1 <- foo %>% 
  ggplot(aes(holiday_flg, fill = holiday_flg)) +
  geom_bar() + 
  theme(legend.position = 'none')

p2 <- foo %>% 
  filter(date > ymd('2016-04-15') & date < ymd('2016-06-01')) %>% 
  ggplot(aes(date ,holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = 'none') +
  labs(x = '2016 date')

p3 <- foo %>% 
  filter(date > ymd('2017-04-15') & date < ymd('2017-06-01')) %>% 
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = 'none') +
  labs(x = '2017 date')

layout <- matrix(c(1,1,2,3),2,2,byrow=FALSE)
multiplot(p1, p2, p3, layout = layout)
```

결과:

- 2016년 4월 말과 5월은 2017년과 휴일이 같다.

- data에서 휴일의 비율은 대략 7%이다.

```{r}
holidays %>% group_by(holiday_flg) %>% count() %>% spread(holiday_flg,n) %>% mutate(frac = `TRUE`/(`TRUE`+`FALSE`))
```


## 4.7 Test data set

train & test data set의 기간 범위를 나타낸다
```{r}
foo <- air_visits %>% 
  rename(date = visit_date) %>% 
  distinct(visit_date) %>% 
  mutate(dset = 'train')

bar <- test %>% 
  separate(id, c('foo', 'bar', 'date'), sep = '_') %>% 
  mutate(date = ymd(date)) %>% 
  distinct(date) %>% 
  mutate(dset = 'test')

foo <- foo %>% 
  bind_rows(bar) %>% 
  mutate(year = year(date))

year(foo$date) <- 2017

foo %>% 
  filter(!is.na(date)) %>% 
  mutate(year = fct_relevel(as.factor(year), c('2017', '2016'))) %>% 
  ggplot(aes(date, year, color = dset)) +
  geom_point(shape = '|', size = 10) +
  scale_x_date(date_labels = '%B', date_breaks = '1 month') +
  #scale_y+reverse() +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9)) +
  labs(color = 'Data set') +
  guides(color = guide_legend(override.aes = list(size = 4, pch = 15)))

```
