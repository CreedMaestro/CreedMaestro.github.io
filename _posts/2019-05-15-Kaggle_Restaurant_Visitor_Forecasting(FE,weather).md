---
title: "캐글 Restaurant Visitor Forecasting Forecasting method & Weather data"
author: "Maestro"
date: "2019/05/15"
categories: Kaggle
tags: Kaggle, 캐글, 고객예측, Restaurant, TimeSeries, 시계열
layout: post
output: 
  md_document:
    variant: markdown_github
---



## 8.3 Holt-Winters

보다 전통적인 시계열 필터링 및 예측은 'stats' 패키지에 구현된 'Holt-Winters' 알고리즘이다. 이건 이동 평균을 사용하여 데이터에 트렌드가 존재함을 고려하는 지수 평활법이다. 여기에선 피팅 및 플로팅 함수에 기본 계절 모델을 정의한다.

```{r}
plot_hw_air_id <- function(air_id){
  
  pred_len <- test %>% 
    separate(id, c('air', 'store_id', 'date'), sep = '_') %>% 
    distinct(date) %>% 
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

  foo <- air_visits %>% 
    filter(air_store_id == air_id)

  visits <- foo %>% 
    right_join(all_visits, by = 'visit_date') %>% 
    mutate(visitors = log1p(visitors)) %>% 
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>% 
    rownames_to_column()
  

  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)

  hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))

  hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>% 
    as.tibble() %>% 
    bind_cols(visits_valid)

  visits_train %>% 
    ggplot(aes(visit_date, visitors)) +
    geom_line() +
    geom_ribbon(data = hw_visits, aes(x = visit_date, ymin = lwr, ymax = upr), fill = 'light blue') +
    geom_line(data = hw_visits, aes(visit_date, visitors), color = 'grey60') +
    geom_line(data = hw_visits, aes(visit_date, fit), color = 'blue') +
    geom_line(data = hw_visits, aes(visit_date, fit), color = 'blue') +
    labs(x = 'Time [weeks]', y = 'log1p visitors vs predictions') +
    ggtitle('HoltWinters')

}
```

이 기능을 예시 시계열에 적용해보자.

```{r}
plot_hw_air_id('air_ba937bf13d40fb24')
```

그리고 상기의 prophet에 대해서도 동일한 예측을 하고 있다.

```{r}
p1 <- plot_hw_air_id('air_f3f9824b7d70c3cf')
p2 <- plot_hw_air_id("air_8e4360a64dbd4c50")
p3 <- plot_hw_air_id("air_1c0b150f9e696a5f")
p4 <- plot_hw_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
```

결과:

- 알고리즘은 'auto.arima'와 유사하게 주어진 충분한 데이터를 합리적으로 잘 수행하지만, 데이터가 누락된 특별한 경우는 분해된다. 누락값을 중위값으로 대체하는 대신 버림으로써 더 나은 성과를 얻을 수 있을 것이다. 그러나 첫인상은 prophet함수의 접근보다 덜 효율적으로 보인다.

- 이 커널을 포크하고 수정한 첨가 및 지수 모델을 실험하여 Holt-Winters 방법의 성능을 조정하라.


## 8.4 timetk

'timetk'패키지는 시계열 분석을 위해 최근에 출시된 툴이다. 'tidyverse' 패키지에 통합되어 있으며, 깔끔한 'tibble' 데이터 프레임과 함께 작동하도록 특별히 설계되어 있다. 여기선 'timetk' 접근 방법과 데이터 적용 방법을 간략하게 설명한다.

다른 예측 도구와 동일한 방식으로 train 및 validation 데이터 프레임을 만들겠다.

```{r}
air_id = "air_ba937bf13d40fb24"


pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()



max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))



foo <- air_visits %>%
  filter(air_store_id == air_id)



visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)



visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)
```

그 후, 'tk_augment_timeseries_signature' 도구를 써서 시계열 특성으로 데이터 프레임을 확장하겠다. 날짜에서 추출한 포괄적인 시계열 특성을 추가하겠다는 것이다. 이런 새로운 특징에는 월, 일, 한 해의 주, 반기 및 분기가 포함된다. 즉, 날짜 시간 데이터의 경우 분 및 초로 감소한다. 향상된 training data를 보자
```{r}
visits_train_aug <- visits_train %>% 
  tk_augment_timeseries_signature()

visits_valid_aug <- visits_valid %>% 
  .$ds %>% 
  tk_get_timeseries_signature()

glimpse(visits_train_aug)
```

이제 'timetk'의 이면에 있는 아이디어는 회귀 또는 분류 접근법에 기초한 예측을 위해 이런 새로운 특징들(선형/논리적 회귀 또는 boosted/ensembled tree와 같은 표준 도구)을 사용하는 것이다. 이 예에서는 간단한 선형 모델을 사용할 것이다. 이 접근 방식은 더 정교한 방법들로 쉽게 확장될 수 있다.

```{r}
fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))
```

그 후 표준 예측 도구를 써서 모델을 예측 범위에 적용하겠다. training 데이터 프레임과 동일한 기능을 포함해야 한다.
```{r}
pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))

pred_tk <- tibble(
  date = visits_valid$ds,
  value = pred
)
```

이전과 마찬가지로 헬퍼 함수에서 예측을 플로팅 방법과 결합한다.

```{r}
plot_tk_lm_air_id <- function(air_id){

    pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()

  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
          ds = visit_date)

  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)

  # augment train with ts info

  visits_train_aug <- visits_train %>%
    tk_augment_timeseries_signature()

  # fit lm

  fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))

  # augment valid with ts info

  visits_valid_aug <- visits_valid %>%
    .$ds %>%
    tk_get_timeseries_signature()

  # predict from lm

  pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))


  pred_tk <- tibble(
      date  = visits_valid$ds,
      y_pred = pred
      )

  # plot

  p <- pred_tk %>%
    ggplot(aes(date, y_pred)) +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    geom_line(colour = "blue") +
    labs(title = str_c("timetk for ", air_id))

  return(p)

}

```

기본 예제 시계열에 적용하겠다.
```{r}
plot_tk_lm_air_id('air_ba937bf13d40fb24')
```

결과:

- 예측을 위해 단순한 선형 모형을 사용했다는 걸 고려하면, 결과는 꽤 괜찮은 것 같다. 더 정교한 방법들은 더 나은 결과를 줄 것이다.

- 이 방법은 기술적으로 시계열 방식이 아니다. 대신 'timetk' 함수를 써서 데이터의 시계열 특성을 추출하고 회귀형 접근 방식에서 추가 기능으로 사용하겠다. 따라서 이 방법은 주로 (자동화 된)리쳐 엔지니어링으로 간주 될 수 있다. 다양한 기능을 선택하여 실험해 보는 게 좋다.


# 9 Weather data

세부적인 날씨 데이터를 위해 [일본 기상청 공식 사이트]('http://www.jma.go.jp/jma/indexe.html')에서 [추출]('https://www.kaggle.com/c/recruit-restaurant-visitor-forecasting/discussion/45874#258792')하여 [제공]('https://www.kaggle.com/c/recruit-restaurant-visitor-forecasting/discussion/46056')한 [Hunter McGushion]('https://www.kaggle.com/huntermcgushion')의 [데이터셋]('https://www.kaggle.com/huntermcgushion/rrv-weather-data')을 쓰겠다.

데이터는 기상 파일 폴더에 있는 csv파일의 형태로 나오고, 여기엔 모든 기상 관측소의 메타 데이터와 위치, 그리고 자세한 기상 데이터의 시계열이 포함되어 있다. 이 파일은 각 상점(지역)위치에 가장 가까운 기상 관측소를 식별하고 이 정보는 수정된 파일에 추가되어 있다.

여기선 기상 관측소와 여기서 제공된 매개 변수에 대한 간략한 개요를 제공할 것이다.

## 9.1 Weather stations

개요 데이터부터 살펴보자. 

```{r}
weather_stations <- as.tibble(fread(str_c(wpath,'weather_stations.csv')))
```

```{r}
summary(weather_stations)
```

```{r}
glimpse(weather_stations)
```

결과:

- 약 1700곳의 기상 관측소가 있다. 각 관측소에는 'prefecture', 'first_name', 'second_name' 3개로 구성된 id를 가지고 있고, 이건 레스토랑 데이터와 매우 유사하다. 또한 고도 데이터가 있어 중요한 고도에서 관측소를 제거하는 데 유용할 수 있다.

- 가장 중요한 정보는 관측소의 경도와 위도 좌표뿐만 아니라 관측소가 폐쇄된 날짜이다. 여기선 training과 예측 날짜 범위 동안 기상 데이터가 있는 관측소만 사용하는게 좋다.

종료 날짜:
```{r}
weather_stations %>% 
  filter(date_terminated != '') %>% 
  mutate(date_terminated = ymd(date_terminated)) %>% 
  ggplot(aes(date_terminated)) +
  geom_histogram(bins = 30, fill = 'blue') +
  ggtitle('Weather station termination date')
```

대다수의 관측소가 과거 데이터만 가지고 있지만, 그것들을 모두 개요 표에 포함하는 게 데이터 셋의 더 넓은 맥락에 대한 느낌을 얻는 데 유용할 것이다.

파란색 마커로 표시된 모든 활성 관측소가 있는 interactive leaflet map이 있다. 여기에 'air', 'hpg'위치를 'air'는 보라색으로, 'hpg'는 빨간색으로 더해서 나타내겠다.각 마커/서클은 각각의 id가 라벨로 표시됐다. 이 지도는 어떤 관측소가 식당과 가까운가를 보여주고 있고, 더 큰 데이터 셋에 이미 포함되어 있는 가장 가까운 관측소의 식별을 확인할 수 있도록 해준다.

```{r}
foobar <- weather_stations %>%
  filter(date_terminated != "")



foo <- air_store %>% select(air_store_id, latitude, longitude) %>% mutate(dset = "air")
bar <- hpg_store %>% select(hpg_store_id, latitude, longitude) %>% mutate(dset = "hpg")



leaflet(foobar) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude, popup = ~id, label = ~id, group = "Weather") %>%
  addCircleMarkers(~foo$longitude, ~foo$latitude, group = "AIR",
                   color = "purple", fillOpacity = 0.5, label = ~foo$air_store_id) %>%
  addCircleMarkers(lng = bar$longitude, lat = bar$latitude, group = "HPG",
                   color = "red", fillOpacity = 0.5, label = ~bar$hpg_store_id)
```

## 9.2 Weather time series

모든 관측소의 공간적 개요를 얻은 후, 제공된 기상 데이터의 예를 살펴보겠다. 모든 특징에 대한 데이터가 있는 'tokyo__tokyo-kana__tonokyo.csv'를 보자.
```{r}
weather_data <- as.tibble(fread(str_c(wdpath,'tokyo__tokyo-kana__tonokyo.csv')))
```

```{r}
summary(weather_data)
```
```{r}
glimpse(weather_data)
```

결과:

- 517일 동안의 train과 test셋에 대한 일별 자료가 있다. 여기엔 기온, 비, 눈, 기압, 햇빛이나 구름 등의 정보가 포함된다.

- 대부분의 feature에서 결측치가 있다. 이 특정한 기상 관측소 데이터 셋은 더 완전한 것들 중 하나이고, 다른 관측소에선 더 많은 결측치를 찾을 수 있다.(예: 'miyagi__shiogama-kana__shioogama.csv') 일부 관측소는 기본적으로 완전한 feature 데이터를 가지고 있는 것으로 나타난다.(예: 'hokkaido_ishikari__sapporo-katakana__satporo.csv') 이런 이질성으로 인해 첫 번째 모델링 접근법에서 전반적인 공통 feature에 중점을 둘 필요가 있다.

약간의 포맷으로 월 또는 요일과 같은 날짜 feature들을 추가하겠다.

```{r}
weather_data <- weather_data %>% 
  mutate(date = ymd(calendar_date),
         wday = wday(date, label = TRUE, abbr = TRUE),
         month = month(date, label = TRUE, abbr = TRUE),
         week = week(date)) %>% 
  select(-calendar_date)
```

그 후에 높은 온도와 평균 습도에 대한 월별 통계를 작성하겠다. 이건 ridgeline 플롯을 위한 완벽한 작업이며, 우리는 'ggridges' 패키지의 효율적인 CRAN vignette에 따라 약간의 스타일링을 추가한다.

```{r}
#library('viridis')
p1 <- weather_data %>% 
  ggplot(aes(x = high_temperature, y = fct_rev(month), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1., bandwidth = 1.4) +
  scale_fill_viridis(name = 'T_max [°C]', option = 'C') +
  ggtitle('최고 온도 관측소 - \ntokyo tokyo-kana tonokyo in 2016/17') +
  labs(x = '고온', y = '') +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(legend.position = 'none') +
  theme(axis.title.y = element_blank())

p2 <- weather_data %>% 
  ggplot(aes(x = avg_humidity, y = fct_rev(month), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1., bandwidth = 4) +
  scale_fill_continuous(low = "white", high = "dark blue") +
  ggtitle('평균 습도 관측소 - \ntokyo tokyo-kana tonokyo in 2016/17') +
  labs(x = '습도', y = '', fill = 'Humidity') +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(legend.position = 'none') +
  theme(axis.title.y = element_blank())

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)
```

도쿄의 여름은 꽤 덥고 습도가 높다는 것을 알게 되었다.겨울과 여름은 평균 온도 20도 차이, 그 외 계절은 10도 정도로 차이가 보인다. 습도는 매달 분산이 더 크고, 겨울과 여름의 차이도 여전히 크다.

그 밖의 기상 정보로는 강수량, 전체 및 가장 깊은 강수량(이 관측소의 경우 세 곳 모두 대부분 누락), 일조 시간, 평균 풍속, 다양한 압력, 구름, 태양 복사열 등이 있다. 여기에 마저 나타내겠다.
```{r}
p1 <- weather_data %>% 
  ggplot(aes(fct_rev(month), hours_sunlight, fill = fct_rev(month))) +
  geom_boxplot() +
  coord_flip() + 
  theme(legend.position = 'none') +
  labs(x = 'Month', y = '일조시간')

p2 <- weather_data %>% 
  ggplot(aes(fct_rev(month), cloud_cover, fill = fct_rev(month))) +
  geom_boxplot() + 
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = 'Month', y='구름')

p3 <- weather_data %>% 
  ggplot(aes(fct_rev(month), precipitation, fill = fct_rev(month))) +
  geom_boxplot() + 
  coord_flip() +
  theme(legend.position = 'none') +
  scale_y_log10() +
  labs(x = 'Month', y = '강수량')

p4 <- weather_data %>% 
  ggplot(aes(fct_rev(month), avg_local_pressure, fill = fct_rev(month))) +
  geom_boxplot() + 
  coord_flip() +
  theme(legend.position = 'none') +
  scale_y_log10() +
  labs(x = 'Month', y = '평균 기압')

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout = layout)
```

결과:

- 흥미로운건, 9월엔 12월보다 일조시간이 더 적다. 이건 구름으로 설명할 수 있는데, 이는 겨울의 구름이 다른 계절보다 상당히 적다는 것을 보여준다. 구름값은 0 ~ 10 사이의 상대적 눈금으로 측정된다.

- 모든 상자가 일정하고 한달 안에 큰 차이가 있지만 전체 강수량은 2월이 가장 낮다. 기억해야 할 건 강수량 feature에 결측치가 많다는 것이다.

- 평균 기압은 여름 동안 분명히 떨어지고 8월은 가장 낮은 값을 갖는다.

# 10 Ref


[원문 링크, original by Heads or Tails]("https://www.kaggle.com/headsortails/be-my-guest-recruit-restaurant-eda/report")
[날씨 자료 링크, Wiki Traffic Forecast Exploration]("https://www.kaggle.com/headsortails/wiki-traffic-forecast-exploration-wtf-eda")
[ARIMA model by Timlee]("https://www.kaggle.com/timolee/feeling-hungry-a-beginner-s-guide-to-arima-models")
