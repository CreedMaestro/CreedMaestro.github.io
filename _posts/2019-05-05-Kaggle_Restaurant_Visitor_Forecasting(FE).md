---
title: "캐글 Restaurant Visitor Forecasting Feature Engineering with R (한글 번역)"
author: "Maestro"
date: "2019/05/05"
categories: Kaggle
tags: Kaggle, 캐글, 고객예측, Restaurant, TimeSeries, 시계열
layout: post
output: 
  md_document:
    variant: markdown_github
---

# 6 Feature engineering

 유의미한 인사이트를 알아낼 수 있는 feature들을 만들거나, 기존에서 추출해 보자. 이건 'date' 컬럼에서 월이나 요일을 추출하는 것만큼 간단할 수도 있고, ('Fig.1'에서 이미 한 것처럼) 여러 연관된 변수들의 상호 작용으로 더 복잡할 수도 있다. 이번 섹션에선 이런 새로운 feature들을 모아서 연구해보자.

개인적인 선호는 모든 engineered 된 feature들을 하나의 코드 블럭으로 모으는 것이다. 커널의 다른 곳에서 다시 검색할 필요 없이. 이건 이따끔씩 특정 중간 변형을 재사용할 수 있는 기능을 더 쉽게 만든다. 분석을 확장함에 따라, 이 코드 블럭으로 돌아갈 것이고 이건 feature space가 커질수록 확장 될 것이다.

```{r}
air_visits <- air_visits %>% 
  mutate(wday = wday(visit_date, label=TRUE),
         wday = fct_relevel(wday, c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
         month = month(visit_date, label = TRUE))

air_reserve <- air_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday , c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day'))

hpg_reserve <- hpg_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day'))

# count stores in area
air_count <- air_store %>% 
  group_by(air_area_name) %>% 
  summarise(air_count = n())

hpg_count <- hpg_store %>% 
  group_by(hpg_area_name) %>% 
  summarise(hpg_count = n())

# distances
med_coord_air <- air_store %>% 
  summarise_at(vars(longitude:latitude), median)
med_coord_hpg <- hpg_store %>% 
  summarise_at(vars(longitude:latitude), median)

air_coords <- air_store %>% 
  select(longitude, latitude)
hpg_coords <- hpg_store %>% 
  select(longitude, latitude)

air_store$dist <- distCosine(air_coords, med_coord_air)/1e3
hpg_store$dist <- distCosine(hpg_coords, med_coord_hpg)/1e3

# apply counts, dist: add prefecture

air_store <- air_store %>% 
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1, 
    dist < 300 ~ 2, 
    dist < 500 ~ 3, 
    dist < 750 ~ 4, 
    TRUE ~5))) %>% 
  left_join(air_count, by = 'air_area_name') %>% 
  separate(air_area_name, c('prefecture'), sep = ' ', remove = FALSE)

hpg_store <- hpg_store %>% 
    mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1, 
    dist < 300 ~ 2, 
    dist < 500 ~ 3, 
    dist < 750 ~ 4, 
    TRUE ~5))) %>% 
  left_join(hpg_count, by = 'hpg_area_name') %>% 
  separate(hpg_area_name, c('prefecture'), sep = ' ', remove = FALSE)
```

## 6.1 Days of the week & months of the year

'visit_date'에서 월별, 일별 feature를 만들어 보겠다. 이전 'Fig. 1'에서 이미 일별, 월별 총고객을 보았고, 여기선 log1p로 변환한 고객수를 보겠다. 우선 일별, 월별 평균과 표준 편차를 계산하고 ridgeline 밀도 분포와 함께 그려 보겠다.

```{r}
p1 <- air_visits %>% 
  group_by(wday) %>% 
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>% 
  ggplot(aes(wday, mean_log_visitors, color = wday)) +
  geom_point(size = 4) + 
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = 'none')

p2 <- air_visits %>% 
  mutate(visitors = log1p(visitors)) %>% 
  ggplot(aes(visitors, wday, fill = wday)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = 'none') +
  labs(x = 'log1p(visitors)', y = '')

p3 <- air_visits %>% 
  group_by(month) %>% 
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>% 
  ggplot(aes(month, mean_log_visitors, color = month)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = month), width = 0.5, size = 0.7) +
  theme(legend.position = 'none')

p4 <- air_visits %>% 
  mutate(visitors = log1p(visitors)) %>% 
  ggplot(aes(visitors, month, fill = month)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = 'none') +
  labs(x = 'log1p(visitors)', y = '')

layout <- matrix(c(1,2,3,4),2,2, byrow = TRUE)
multiplot(p1, p2, p3, p4, layout = layout)
```

결과:

- 일별 mean_log_visitor수의 차이는 해당 불확실성보다 훨씬 작고, 이건 옆의 밀도 차트의 뚜렷한 중첩으로 확인할 수 있다. 그럼에도 주말에는 방문객 수의 증가 경향을 볼 수 있다.

- 월별 역시 12월의 증가한 수를 포함하더라도, 크게 다르지 않다.

장르별 내역 상세:
```{r}

air_visits %>% 
  left_join(air_store, by = 'air_store_id') %>% 
  group_by(wday, air_genre_name) %>% 
  summarise(mean_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>% 
  ggplot(aes(wday, mean_log_visitors, color = wday)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust =1, vjust = 0.9)) +
  facet_wrap(~ air_genre_name)
```

결과:

- 다른 장르는 요일에 큰 영향이 없지만('Japanese food'와 같은), 'Karaoke' or 'international Cuisine'과 같은 장르는 주말 &  주중에 따른 큰 영향이 있다.

- 전반적으로, 어느 요일에도 장르에 영향을 미치지 않는다.

방문자 vs 예약 후 방문자의 통계는 어떻게 될지, 요일별 차이가 있는지 boxplot으로 알아보겠다.
```{r}

all_reserve %>% 
  mutate(wday = wday(visit_date, label = TRUE),
         wday = fct_relevel(wday, c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'))) %>% 
  ggplot(aes(wday, visitors - reserve_visitors, fill = wday)) +
  geom_boxplot() +
  theme(legend.position = 'none')
```

결과:

- 주말 예약, 특별히 일요일에 비해 방문자 수가 더 적지만 큰 차이는 없다는 걸 알 수 있다.

## 6.2 Restaurant per area - air/hpg_count

여기선 특정 지역에서 얼마나 많은 레스토랑을 찾을 수 있는지에 대한 질문과 지역당 레스토랑의 수가 평균 고객수에 영향을 미치는지를 다룬다.

이걸 알아보기 위해 해당 지역의 'air/hpg restaurants'의 수를 비교할 것이다. 분포 결과 차트를 나타낼 것이고, 식당별 mean log1p - 변환된 고객수를 추정할 것이다.그리고 숫자별 그룹을 지어 지역당 평균과 표준 편차를 계사한 것이다.
```{r}
p1 <- air_count %>% 
  ggplot(aes(air_count)) +
  geom_histogram(binwidth = 2, fill = 'blue')

p2 <- hpg_count %>% 
  ggplot(aes(hpg_count)) +
  geom_histogram(binwidth = 5, fill = 'red')

p3 <- air_visits %>% 
  left_join(air_store, by = 'air_store_id') %>% 
  group_by(air_store_id, air_count) %>% 
  summarise(mean_store_visit = mean(log1p(visitors))) %>% 
  group_by(air_count) %>% 
  summarise(mean_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>% 
  ggplot(aes(air_count, mean_log_visitors)) +
  geom_point(size = 4, color = 'blue') +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors),
                color = 'blue', width = 0.5, size = 0.7) +
  geom_smooth(method = 'lm', color = 'black') +
  labs(x = 'Air restaurants per area')

layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout = layout)

```

결과:

- 'air' data에서 대부분의 지역은 레스토랑의 수가 적고, 지역당 2곳이 가장 높은 분포다. 이건 지역당 단일 장르를 볼 수  없다는 이전의 관측치로 돌아간다. 항상 최소 2개의 동일 타입의 장르가 있다. 'air' 데이터에는 16-20 주위의 수에 2번째 피크가 있다. data의 수가 더 큰 hpg 또한 지역당 레스토랑의 분포가 작은 곳의 수치가 높고, 점점 감소 경향아래 작은 피크도 거의 없다.

- 지역당 mean log1p 고객수는 불확실성이 크고, 모두 서로 일치한다. 검은색 선형 회귀 분석선에서 보이는 트렌드는 지역당 식당의 분포가 많은 곳에 평균 고객수가 적은 것을 알 수 있지만 그 경향은 아주 약하다.

## 6.3 Distance from the busiest area

데이터에 공간 좌표가 있으면 자동으로 이 좌표 사이의 거리도 가진다. 첫번째 근사값으로 두 점 사이의 선형(직선 방향으로) 거리를 사용한다.

이 거리를 계산하기 위해 'geosphere' 패키지의 'distCosine' 함수를 사용한다. 이 방법은 구형의 지구에서 두 점 사이의 최단 거리를 제공하는데, 이런 국지적 분석을 위해 지구 모양의 타원형 왜곡을 무시하기로 한다.

모든 거리에 대한 단일 참조 점으로서 위도 및 경도의 중위값을 선택한다. 이 분석은 동일한 방법론으로 두 식당 사이의 모든 쌍방향 거리를 조사하기 위해 확장할 수 있다. 이 조사를 커널에서 하지는 않겠지만, 원하는 사람들이 있다면 이런 방식으로 도움이 될 통찰력을 얻을 수 있는지 알아보길 권한다.

'air', 'hpg' 데이터의 모든 레스토랑의 중위값으로 부터 거리를 히스토그램으로 나타낸다. 이 분포는 거리별로 5개의 다른 빈으로 그룹화하고, 5개 그룹의 mean log1p 고객수를 비교할 것이다.

```{r}

p1 <- air_store %>% 
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = 'blue') +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = 'Linear distance [km]')

p2 <- hpg_store %>% 
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = 'red') +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = 'Linear distance [km]')

p3 <- air_visits %>% 
  left_join(air_store, by = 'air_store_id') %>% 
  group_by(air_store_id, dist_group) %>% 
  summarise(mean_store_visit = mean(log1p(visitors))) %>% 
  group_by(dist_group) %>% 
  summarise(mean_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>% 
  ggplot(aes(dist_group, mean_log_visitors)) +
  geom_point(size = 4, color = 'blue') +
  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,
                    ymax = mean_log_visitors + sd_log_visitors),
                color = 'blue', width = 0.5, size = 0.7) +
  labs(x = 'Distance grouping')

lyout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1, p2, p3, layout = layout)
```

결과:

- 거리의 분포는 매우 짧은 값에서 분명한 피크(참조가 중위값이기 때문에 예상된)를 가지고 있다. 또한 약 450km와 800km 사이에서 더 많은 봉우리들을 볼 수 있다. 이것들은 도시의 그룹에 해당하며, 'air'와 'hpg'데이터 모두 일관된다. 80,300, 500, 750km의 수직 흑색 선을 기준으로 5개의 거리 빈을 정의한다.

- 이 5개의 거리 빈에 대한 mean log1p 변환 고객수는 표준 편차 내에서 유의한 차이를 보이지 않는다.

'air'(파란색)과 'hpg'(빨간색) 식당의 leaflet map을 표시하고, 원의 크기는 (어두운 녹색) 중심 참조 좌표를 기준으로 거리 빈이 증가할수록 커진다. 큰 놀라움 없이, 중앙 위치는 도쿄에서 찾을 것이다.

```{r}
foo <- air_store %>% select(latitude, longitude, dist_group) %>% mutate(dest = 'air')
bar <- hpg_store %>% select(latitude, longitude, dist_group) %>% mutate(dest = 'hpg')

leaflet(foo) %>% 
  addTiles() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addScaleBar() %>% 
  addCircleMarkers(~longitude, ~latitude, group = 'AIR',
                   color = 'blue', fillOpacity = 0.5, radius = 3*foo$dist_group) %>% 
  addCircleMarkers(lng = bar$longitude, lat = bar$latitude, group= 'HPG',
                   color = 'red', fillOpacity = 0.5, radius = 3*bar$dist_group) %>% 
  addCircleMarkers(lng = med_coord_air$longitude, lat = med_coord_air$latitude, group = 'Centre',
                   color = 'darkgreen', fillOpacity = 1) %>% 
  addLayersControl(
    overlayGroups = c('AIR','HPG','Centre'),
    options = layersControlOptions(collapsed = FALSE)
  )
```

## 6.4 Prefectures

이것은 지역을 지역 명의 첫 번째 부분으로 정의하는('현' 단위 표기) 단순한 기능이다. discussion에서 'Oscar Takeshita'가 설명한 지역 이름을 일관된 서식으로 사용한다.(몇 가지 주의 사항을 확인바란다.) 이는 상대적으로 소수로 구별되는 높은 수준 영역을 제공한다.

```{r}
p1 <- air_store %>%

  ggplot(aes(prefecture)) +

  geom_bar(fill = "blue") +

  coord_flip() +

  ggtitle("air prefectures - # restaurants") +

  labs(x = " ")



p2 <- hpg_store %>%

  ggplot(aes(prefecture)) +

  geom_bar(fill = "red") +

  coord_flip() +

  ggtitle("hpg prefectures - # restaurants") +

  labs(x = " ")

p3 <- air_visits %>%

  left_join(air_store, by = "air_store_id") %>%

  group_by(air_store_id, prefecture) %>%

  summarise(mean_store_visit = mean(log1p(visitors))) %>%

  group_by(prefecture) %>%

  summarise(mean_log_visitors = mean(mean_store_visit),

            sd_log_visitors = sd(mean_store_visit)) %>%

  ggplot(aes(prefecture, mean_log_visitors)) +

  geom_point(size = 4, color = "blue") +

  geom_errorbar(aes(ymin = mean_log_visitors - sd_log_visitors,

                    ymax = mean_log_visitors + sd_log_visitors),

                    color = "blue", width = 0.5, size = 0.7) +

  labs(x = "prefecture") +

  theme(axis.text.x  = element_text(angle=15, hjust=1, vjust=0.9))

layout <- matrix(c(1,2,1,2,1,2,3,3,3,3),5,2,byrow=TRUE)
multiplot(p1, p2, p3, layout = layout)
```

결과:

- 'air'데이터에는 9개의 현이, 'hpg'데이터에는 13개의 현이 있다. 두 데이터 모두 'Tokyo'에 가장 많은 수가 있다.

- 여기서도 'air'의 9개 현의 평균 고객수에서 특별히 다른점을 찾지 못하겠다. 가능한 모든 변형은 error bar의 크기보다 작다.

[캐글 번역 링크, Restaurant_visitor_Forecasting]("https://www.kaggle.com/maestroyi/restaurant-visitor-forecasting-eda-with-r?scriptVersionId=14464395")

[원저자 링크]("https://www.kaggle.com/headsortails/be-my-guest-recruit-restaurant-eda/report")