---
title: "캐글 Restaurant Visitor Forecasting Time Series parameters & Forecasting method"
author: "Maestro"
date: "2019/05/10"
categories: Kaggle
tags: Kaggle, 캐글, 고객예측, Restaurant, TimeSeries, 시계열
layout: post
output: 
  md_document:
    variant: markdown_github
---

# 7 Time series parameters

여러 식당의 지리적 특성이나 요리적 특성에 따른 new feature 생성 후, 고객수에 따른 시계열을 보겠다. 829개의 'air' restaurnat는 829개의 시계열이 있고 그것들 각각을 개별적으로 보는 것은 실제로 쉽게 가능하다. 그리고 데이터에 대한 느낌을 얻기 위해 작은 부분이나 큰 부분을 그리는 커널이 있다.

다른 접근법을 취해서 각 시계열의 메타 parameter를 살펴볼 것이다. 이런 feature들은 시계열의 평균, 표준편차, 기울기 및 기울기 오류이다. 각 parameter는 log1p로 변환하여 계산될 것이다. 각 feature를 추출하여 정의하고 각 식당에 적용해볼 것인데 데이터 셋의 크기가 작아 시간이 거의 걸리지 않을 것이다. 

```{r}
foo <- air_visits %>% 
  left_join(air_store, by = 'air_store_id') %>% 
  group_by(air_store_id, air_genre_name) %>% 
  summarise(mean_log_visits = mean(log1p(visitors)),
            mean_log_visits = mean(log1p(visitors)),
            sd_log_visits = sd(log1p(visitors))) %>% 
  ungroup()

params_ts1 <- function(rownr){
  bar <- air_visits %>% 
    filter(air_store_id == foo$air_store_id[rownr])
  slope <- summary(lm(visitors ~ visit_date, data = bar))$coef[2]
  slope_err <- summary(lm(visitors ~ visit_date, data = bar))$coef[4]
  
  foobar <- tibble(
    air_store_id = foo$air_store_id[rownr],
    slope = slope,
    slope_err = slope_err
  )
  
  return(foobar)
}

params <- params_ts1(1)
for (i in seq(2, nrow(foo))){
  params <- bind_rows(params, params_ts1(i))
}

ts_params <- foo %>% 
  left_join(params, by = 'air_store_id')
```

parameters와 1차원 및 2차원 분포도를 표시하겠다.

```{r}
p1 <- ts_params %>% 
  ggplot(aes(mean_log_visits)) +
  geom_histogram(bins = 50, fill = 'blue')

p2 <- ts_params %>% 
  ggplot(aes(sd_log_visits)) +
  geom_histogram(bins = 50, fill = 'blue')

p3 <- ts_params %>% 
  filter(slope < 0.5) %>% 
  ggplot(aes(slope)) +
  geom_histogram(bins = 50, fill = 'blue') +
  labs(x = 'slope < 0.5')

p4 <- ts_params %>% 
  ggplot((aes(mean_log_visits, sd_log_visits))) +
  geom_point(size = 2, color = 'blue')

p5 <- ts_params %>% 
  ggplot((aes(slope, slope_err))) +
  geom_point(size = 2, color = 'blue')

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow = TRUE)
multiplot(p1, p2, p3, p4, p5, layout = layout)
```

결과:

- log1p 변환된 고객 수에 대한 평균 및 표준 편차의 분포는 상대적으로 넓다. 분포는 비교적 대칭이지만 둘 중 어느 쪽도 평균성이라 가정할 수는 없을 것 같다. 왼쪽 아래 평균 vs 표준편차의 모형은 둘 사이에 관계가 있을 것이라 생각된다. 보면 평균 방문자 수가 많을수록 차이가 작아진다.

- 기울기의 분포가 좁고, '0'에 기울기가 몰려있다. 오른쪽 하단의 기울기 vs 기울기 에러의 차트는 몇몇의 기울기 에러가 큰 값과 극단적으로 절대치의 기울기 값이 큰 이상치가 있음을 보여준다.

```{r}
ts_params %>% 
  filter(abs(slope) > 0.25) %>% 
  select(air_store_id, air_genre_name, slope, slope_err)
```

'ggforce' 패키지에 구현된 'facet zoom view'를 써서 log1p 고객수의 평균과 시계열 기울기 사이의 관계를 종합적으로 볼 것이다. 오른쪽에는 전체 공간이, 왼쪽에는 확대된 버전을 그렸다. 확대 영역은 음영을 어둡게 하여 오른쪽에 표시했다. 이런 방식의 표시는 몇 가지 극단적인 점이 대다수 데이터 클러스터의 특성에 초점을 맞추기가 어려운 경우에 적합하다. 'mean_log_visits'와 'slope'는 장르를 기준으로 표기했고, 'errorbar'는 회색으로 표기했다.

```{r}
ts_params %>%

  ggplot(aes(mean_log_visits, slope, color = air_genre_name)) +
  geom_errorbarh(aes(xmin = mean_log_visits - sd_log_visits,
                    xmax = mean_log_visits + sd_log_visits),
                    color = "grey70", size = 0.7) +

  geom_errorbar(aes(ymin = slope - slope_err,
                    ymax = slope + slope_err),
                    color = "grey70", size = 0.7) +

  geom_point() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 4))) +
  labs(color = "") +
  facet_zoom(y = (slope < 0.05 & slope > -0.1))
```

결과:

- 'log1p' 고객의 평균이 가장 낮은 3값이 모두 'Dining bar'임에도 불구하고 장르별 클러스터링은 많지 않다. 오른쪽의 개요 플롯에서 다시 기울기 이상치가 보이고 모두 평균값이 크다는 걸 알 수 있다.

- 양쪽 차트 모두 높은 기울기가 더 높은 평균과 결부되는 트렌드를 볼 수 있다. 이상할 것 없이, 낮은 고객 수 보다 높은 고객 수에서 더 넓은 범위를 가진 절대 기울기 값을 사용했기 때문이다. 효과의 지속여부를 보려면 평균으로 정규화된 기울기를 자유롭게 표시해보라.

이 차트의 극단치는 이제 자세히 알아볼 수 있고, 그 중 일부를 다음 섹션에서 예로 알아볼 것이다.

# 8 Forecasting methods and examples

여기서는 시계열 예측에 초점을 맞추겠다. 이전에서 data 셋과 여러 feature들을 이미 확인했다. 다음 섹션에선 기본적인 예측 방법들을 소개한다. 이번 챕터에선 저자의 ['WikiTrafficForecast kernel']("https://www.kaggle.com/headsortails/wiki-traffic-forecast-exploration-wtf-eda")에 나온 설명과 방법에서 차용하여 시작한다.

## 8.1 ARIMA / auto.arima

일반적인 예측 방법은 자기진행 통합 이동 평균 모델이다.['ARIMA'의 약어] ("https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average") 이 종류의 모델은 ARIMA(p, d, q)와 같이 세 개의 p, d, q 인덱스에 의해 매개 변수화 된 세 개의 요소로 되어있다.

- auto-regressie / p: 미래 데이터의 회귀 모델을 계산하기 위해 과거 데이터를 쓸 것이다. 매개 변수 p는 지연의 범위를 나타낸다. 예를 들어 ARIMA(3,0,0)은 t에서 값을 계산하기 위한 회귀 분석에서 t-1, t-2및 t-3의 값을 포함한다.

- intergrated / d: 시계열의 현재 값과 이전값을 빼는 횟수를 알려주는 차등화 매개 변수이다. 차등화는 평균을 안정시키고 계절적 트렌드를 제거한다는 점에서 시계열의 변경 사항을 제거한다. 대형 트렌드가 제거되는 경우 지연(예를 들어, t와 t-1 사이의 차이)를 계산하는 것이 가장 중요하다.분산(또는 변동성의 양)(및 자기 공분산)이 시간에 구애받지 않는 시계열(즉, 하루하루 변하지 않음)을 정지라고 한다.

- moving average / p: 이 매개 변수는 모델의 회귀 오류에 포함할 이전의 오차 항 수를 제공한다.

각 개별 시계열에 필요한 ARIMA 매개 변수를 추정하는 auto.arima tool을 쓸 것이다. auto.arima에 데이터를 넣으려면 ts tool을 써서 시계열 객체로 변환해야 한다. 또한 'forecast' 패키지의 'tsclean' 함수를 써서 이상치 제거와 cleaning을 위한 단계를 추가할 것이다. 이전에서 이미 데이터가 모델의 사전 설정된 매개 변수중 하나가 될 강력한 주간 주기를 포함하고 있는 걸 보았다. 데이터를 변환 할 때 이 지식을 포함해서 모든 것을 comment와 설명과 함께 단계별로 설정하고 함수로 바꾸자.

첫 번째 예제는 air_store_id(“air_ba937bf13d40fb24”)이다.

```{r}
air_id = 'air_ba937bf13d40fb24'
```

예측을 테스트하기 위해 최후에 예측할 과제(4월 23일 ~ 5월 31일)과 같은 시간대를 예측할 것이다. 테스트 예측 범위의 길이에서 39일을 자동으로 추출하여 이를 '예측 길이'로 정의할 것이다.

```{r}
pred_len <- test %>% 
  separate(id, c('air', 'store_id', 'date'), sep = '_') %>% 
  distinct(date) %>% 
  nrow()
```

training 샘플의 마지막 39일을 예측하는 걸 선택할 것이다. training date의 가장 마지막 날짜에서 '예측 길이'를 빼 3월 14일 유효성 검정 샘플의 시작을 정의할 것인데, 이건 여기에 없을 수도 있다. 또한 gap이 있는 여러 시계열에 대비하기 위해 모든 'visit_dates'의 데이터 셋을 만들 것이다.

```{r}
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
```

다음은 특정 'air_store_id'의 시계열을 추출한 것이다. log1p로 변환한 고객수를 모든 'visit_date'의 데이터 셋에 join할 것이다. 이건 결측치를 대체할 중위값을 줄 것이다. 중위값은 최고는 아니더라도 합리적인 선택은 될 것이다. 대부분의 시계열 예측 tool은 gap이 없는 순차적 시계열을 필요로 한다. 이것이 이 단계에서 만드는 것이다.

```{r}
foo <- air_visits %>% 
  filter(air_store_id == air_id)

visits <- foo %>% 
  right_join(all_visits, by = 'visit_date') %>% 
  mutate(visitors = log1p(visitors)) %>% 
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>% 
  rownames_to_column()
```

새로운 시계열을 써서, 'training'과 'validation'으로 데이터를 분할하겠다.

```{r}
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)
```

이제는 피팅을 할 것이다. 전에 말한 'ts' 함수를 써서 시계열 객체를 만들고 'tsclean' tool을 써서 이상치를 제거하고, 주간 빈도를 추가할 것이다. 단계별 및 근사값 매개 변수 설정은 이 tool이 모든 모델 매개 변수에 대해 보다 철저하고 정확한 검색을 수행함을 의미한다. 이러면 계산 시간이 늘어나지만, 데이터 셋이 작을 때는 별 문제가 되지 않는다.

```{r}
arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)
```

적합 ARIMA 모델을 써서 '예측 길이'를 예상하겠다. 신뢰 구간을 포함할 것이다.

```{r}
arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))
```

이제 예측을 차트해보자.'ggplot2'패키지의 'autoplot' 함수를 써서 여기 있는 시계열 객체의 데이터 타입에 따른 차트를 만들겠다. 예상 고객수는 진한 파란색으로 표시되고, 밝은 파란색은 신뢰 범위이다. 실제 유효성 검사 횟수는 회색으로 추가하겠다.

```{r}
arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
  labs(x = "Time [weeks]", y = "log1p visitors vs auto.arima predictions")
```

예측의 첫날들은 꽤 잘맞았지만, 더 큰 급상승들은 포착할 수 없었다. 그래도 다른 방법들과 비교하는건 유용한 방법이다.
이제 이 절차를 플로팅 부분을 포함한 함수로 바꿀 것이다.

```{r}
plot_auto_arima_air_id <- function(air_id){
  
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
  
  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          stepwise = FALSE, approximation = FALSE)
  
  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50, 95))
  
  arima_visits %>% 
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = 'grey40') +
    labs(x = 'Time [weeks]', y = 'log1p visitors vs forecast')
}
```

몇 몇 시계열에 함수를 적용해보겠다. 이전 섹션에서 확인했던 기울기 이상치의 두 값을 포함해서.

```{r}
p1 <- plot_auto_arima_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_auto_arima_air_id("air_8e4360a64dbd4c50")
p3 <- plot_auto_arima_air_id("air_1c0b150f9e696a5f")
p4 <- plot_auto_arima_air_id("air_900d755ebd2f7bbd")

layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
```

결과:

- 상기 패널에 있는 두 개의 시계열은 상당히 완전하지만, 긴 gap이(및 중간 층만)이 주간 주기성을 느슨하게 하는 왼쪽 상단 패널의 예측에 문제를 일으킨다는 것을 알 수 있다. 오른쪽 상단 패널은 이런 주기성을 유지하고 있으며 첫날의 예측은 비교적 양호하지만, 변동의 진폭은 빠르게 예측하지 못했다.

- 아래 패널에는 위 시계열 매개 변수 공간의 이상치 2개가 포함되어 있으며, 여기에서 실제로 문제가 생긴 경우를 볼 수 있다.
이런 종류의 특별한 시계열은 'test' 데이터 셋의 방문 횟수가 충분히 많은 경우, 적절한 예측 알고리즘의 성능이 나빠질 수 있다.

전반적으로 결과는 훌륭하진 않지만 완전 자동 예측(주간 주기만 가정)인 점을 감안하면 auto.arima tool은 다른 방법을 비교할 수 있는 첫 번째 기준점을 제공한다.

더 많은 ARIMA 모델의 세부 사항은 [Timlee]("https://www.kaggle.com/timolee")의 [상기 커널]('https://www.kaggle.com/timolee/feeling-hungry-a-beginner-s-guide-to-arima-models')을 참조 바란다.


## 8.2 Prophet

[prophet]("https://facebook.github.io/prophet/") 예측 tool은 페이스북의 오픈소스 소프트웨어 디벨럽을 사용했다. 'Prophet'은 시계열을 1)선형 / 논리적 경향, 2) 연간 계절 성분, 3)주간 계절 성분 및 4)중요한 요일의 선택적 목록(예: 휴일, 특수 이벤트 등)으로 분해하는 적층 회귀 모델을 사용한다. 이건 '누락 데이터, 추세 변화 및 큰 이상치에 효과가 좋다.특히 이 cometition에서는 누락 데이터 함수는 유용할 수 있다.

tool을 써보자. ARIMA섹션에서 했던 일과 설명은 반복하지 않겠다.

상기 기간(3월 14일 이전/이후)의 'training'과 'validation' 데이터 셋을 다시 만들겠다. ARIMA 방식의 유일한 차이점은 아래와 같다:

- NA를 대체하지 않겠다. 'prophet'은 결측치를 다룰 줄 알기 때문이다.

- 'prophet'은 날짜는 'ds', 시계열 변수는 'y'라는 두 개의 열이 있는 데이터 프레임을 예상한다.

```{r}
air_id = "air_ba937bf13d40fb24"

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
  rownames_to_column() %>% 
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)
```


'prophet' 모델을 써서 예측해 보자

- 'changepoint.prior.scale' 매개 변수는 trend의 유연성을 조정한다. 이 매개 변수를 증가시키면 적합성은 더 유연해지지만 예측 불확실성이 증가하고 소음에도 과적합할 가능성이 높아진다. 이 'changepoints'는 매개 변수를 따로 지정하지 않는 한 자동으로 맞춰진다.(여기서는 자동으로 한다.)

- 'yearly.seasonality' 매개 변수는 정교하게 활성화/비활성화 되어야 하고, 'prophet'이 대규모 사이클을 알아차릴 수 있도록 한다. 우리는 겨우 1년치 데이터를 가지고 있는데, 이건 연간 주기를 찾기에는 불충분하고 아마도 몇 달 동안의 시간 척도의 변화를 식별하기에도 충분하지 않을 것이다. 이 매개 변수의 성능을 자유롭게 테스트 해보라.

```{r}
proph <- prophet(visits_train, changepoint.prior.scale = 0.5, yearly.seasonality = FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast <- predict(proph, future)
```

표준적인 'prophet' 예측 차트를 나타내겠다.

```{r}
plot(proph, fcast)
```

관측 된 데이터는 검은색 점으로 표시되고 적합 모델은 예측과 함께 파란색 선으로 표시된다. 연한 파란색에서 이에 상응하는 불확실성을 볼 수 있다.

'prophet'은 추세, 연간 계절성(포함되는 경우) 및 주간 주기: 모델의 적층 성분을 검사 할 수 있는 분해 플롯을 제공한다.

```{r}
prophet_plot_components(proph, fcast)
```

결과:

- 'prophet'으로 돌려보니 이전에 확인한 것과 비슷한 주간 변화 패턴을 찾았다. 금/토가 다른 요일보다 인기가 있다는 것이다.
그러나 차이점은 일요일의 평균 고객수가 매우 낮다는 것이다. 이건 '도쿄'의 'Dining Bar'인 이 특정 식당의 속성 때문일 것이다. 이 차트에서 볼 수 있는 정도까지는 아니지만, 'Fig.23' 차트에서 일반적으로 'Dining Bars'는 일요일이 바쁘지 않다는 것을 알 수 있다. 일요일과 토요일의 고객 수 차이는 최종 모델의 흥미로운 특징일 수 있다.

- 장기적인 경향은 'Fig.11'에서 나타난 평균적인 작용과는 다른데, 2016년 말에 더 많이 상승할 가능성이 있었다. 여기서 2016년 중반의 피크를 볼 수 있다.

- 참고로 이것은 하나의 특정 시계열이다. 다른 시계열도 확인해 본 결과 그것들 중 다수가 예상한 바를 보여준다.2017년 12월경 장기 피크의 금/토/일 vs 월~ 목 변동을. 커널을 포크해서 자유롭게 실험해보라.

ggplot2 패키지를 잘 쓰면 플로팅 매개 변수를 보다 잘 제어할 수 있다.

```{r}
fcast %>% 
  as.tibble() %>% 
  mutate(ds = date(ds)) %>% 
  ggplot(aes(ds, yhat)) +
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = 'light blue') +
  geom_line(colour = 'blue') +
  geom_line(data = visits_train, aes(ds, y), colour = 'black') +
  geom_line(data = visits_valid, aes(ds, y), colour = 'grey50')
```

나쁘지 않아 보인다. 'training' 고객 수는 검은색, 'validation' 셋은 회색이고 파란색과 연한 파란색은 불확실성을 더한 예측이다.

```{r}
plot_prophet_air_id <- function(air_id){
  
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
  rownames_to_column() %>% 
  select(y = visitors, ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)

proph <- prophet(visits_train, changepoint.prior.scale = 0.5, yearly.seasonality = FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast <- predict(proph, future)

p <- fcast %>% 
  as.tibble() %>% 
  mutate(ds = date(ds)) %>% 
  ggplot(aes(ds, yhat)) +
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = 'light blue') +
  geom_line(color = 'blue') +
  geom_line(data = visits_train, aes(ds, y), color = 'black') +
  geom_line(data = visits_valid, aes(ds, y), color = 'grey50') +
  labs(tibble = str_c('Prophet for ', air_id))

return(p)
}
```

time series 예측을 하겠다.(prophet을 하기 위한 데이터가 충분하지 않아서 'air_900d755ebd2f7bbd'를 제외하고, 'air_820d1919cbecaa0a'로 대체하겠다.)

```{r}
p1 <- plot_prophet_air_id('air_f3f9824b7d70c3cf')
p2 <- plot_prophet_air_id('air_8e4360a64dbd4c50')
p3 <- plot_prophet_air_id('air_1c0b150f9e696a5f')
p4 <- plot_prophet_air_id('air_820d1919cbecaa0a')

layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1, p2, p3, p4, layout = layout)
```

이것들도 나쁘지 않아 보인다. 이 중 최소 3가지에 대해 유연성이 떨어질 수 있는 트렌드 요소를 과대 평가하는 것처럼 보인다.4번째 시계열은 prophet함수가 할 수 있는 데이터가 너무 적다. 이 트렌드 요소를 완전히 버릴 수도 있고, 단순히 중위값을 취할 수도 있다.

Prophet함수는 예측에 휴일과 다른 특별한 사건들을 포함할 수 있는 가능성을 제공한다. 이건 골든위크를 고려하는 데 매우 유용할 수 있다. 골든 위크의 영향에 대한 맞춤형 분석을 통해 더 많은 통찰력을 얻으려면, 'BreakfastPirate'의 [커널]('https://www.kaggle.com/breakfastpirate/weeks-before-after-golden-week-2016")과 관련 [토론]("https://www.kaggle.com/c/recruit-restaurant-visitor-forecasting/discussion/45048")의 [토픽]("https://www.kaggle.com/c/recruit-restaurant-visitor-forecasting/discussion/45120")으로 이 커널을 살펴 보라.

이번 컴퍼티션에서는 휴일 데이터를 쉽게 구할 수 있으므로, 예측 기능에 포함시키고 싶은 prophet함수에 맞게 이름을 바꾸기만 하면 된다. 2016 골든 위크를 예측할 때 휴유일에 따른 차이가 있는지 기능을 수정해보자. 중간 단계에서 2016년 5월 31일의 'air_visits' 데이터를 잘라내기만 하면 된다.

```{r}
plot_prophet_air_id_holiday <- function(air_id, use_hday){
  
  air_visits_cut <- air_visits %>% 
    filter(visit_date <= ymd('20160531'))
  
  hday <- holidays %>% 
    filter(holiday_flg ==TRUE) %>% 
    mutate(holiday = 'holiday') %>% 
    select(ds = date, holiday)
  
  pred_len <- test %>% 
    separate(id, c('air', 'store_id', 'date'), sep = '_') %>% 
    distinct(date) %>% 
    nrow()
  
  max_date <- max(air_visits_cut$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits_cut$visit_date), max(air_visits_cut$visit_date), 1))
  
  foo <- air_visits_cut %>% 
    filter(air_store_id == air_id)
  
  visits <- foo %>% 
    right_join(all_visits, by = 'visit_date') %>% 
    mutate(visitors = log1p(visitors)) %>% 
    rownames_to_column() %>% 
    select(y = visitors, ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  if(use_hday == TRUE){
    proph <- prophet(visits_train,
                     changepoint.prior.scale = 0.5,
                     yearly.seasonality = FALSE,
                     holidays = hday)
    
    ptitle = 'Prophet (w/ holidays) for '
    
  } else {
    
      proph <- prophet(visits_train,
                       changepoint.prior.scale = 0.5,
                       yearly.seasonality = FALSE)
      ptitle = 'Prophet for '
  }
  
  future <- make_future_dataframe(proph, periods = pred_len)
  
  fcast <- predict(proph, future)
  
  p <- fcast %>% 
    as.tibble() %>% 
    mutate(ds = date(ds)) %>% 
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = 'light blue') +
    geom_line(color = 'blue') +
    geom_line(data = visits_train, aes(ds, y), color = 'black') +
    geom_line(data = visits_valid, aes(ds,y), color = 'grey50') +
    labs(title = str_c(ptitle, air_id))
  
  return(p)
}
```

잘 예측된 시계열을 가지고 공휴일의 유무에 따른 예측을 비교해보자.

```{r results = 'hide', fig.align='default', warning=FALSE, fig.cap='Fig. 36', out.width = '100%'}
p1 <- plot_prophet_air_id_holiday('air_5c817ef28f236bdf', TRUE)
p2 <- plot_prophet_air_id_holiday('air_5c817ef28f236bdf', FALSE)

layout <- matrix(c(1,2),2,1,byrow = TRUE)
multiplot(p1, p2, layout=layout)

```

결과:

- 공휴일을 포함하니 골든 위크 고객 예측에 미묘한 개선이 있다. training set에 휴일이 더 포함되면 이 요소의 성능이 개선될 수 있지만, 이것을 검증하는 건 어려워 보인다.
