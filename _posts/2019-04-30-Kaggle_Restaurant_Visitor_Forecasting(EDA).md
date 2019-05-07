---
title: "캐글 Restaurant Visitor Forecasting Feature Relations"
author: "Maestro"
date: "2019/04/30"
categories: Kaggle
tags: Kaggle, 캐글, 캐글코리아, 고객예측, Restaurant
layout: post
output: 
  md_document:
    variant: markdown_github
---



# 5 Feature relations

 개별 data set을 모두 확인해봤다. 이제 결합을 해보자. 여러 특징들의 관계와 이런 특징들이 방문객 수에 어떤 영향을 주는지를 찾아보자.어떤 신호든지 개별 feature 분포의 맥락 안에서 해석해야 한다. 

## 5.1 Visitors per genre

multi-feature space의 첫번째 차트는 'air_genre_name'에 따라 분류된 air restaurants의 평균 방문객 수를 나타낸다.
facet plot을 써서 14개 카테고리의 time series를 나타내보자.

```{r}
foo <- air_visits %>% 
  left_join(air_store, by = 'air_store_id')

foo %>% 
  group_by(visit_date, air_genre_name) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ungroup() %>% 
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() + 
  labs(y = "'air restaurants'의 평균 방문객 수", x = 'Date') +
  theme(legend.position = 'none') +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)
```

결과:

- 일별 평균 방문객은 10~100명 사이에 분포한다.각 카테고리내에서 장기 추세는 안정적으로 보인다.
'Creative Cuisine', 'Okonomiyaki' 외에도 2016년 후반부터 'Asian' 식당의 인기가 감소하는 추세이다.

- 'Karaoke', 'Asina'과 같은 적은 수의 time series는 다른 타입보다 변동성이 심해 보인다. 'Asian' restaurants은 그들의 희소성에도 불구하고 매우 인기가 있어 보인다.

모든 curves에서 익숙한 주간 추이가 보인다. 장르별, 요일별 평균 방문객을 조금 세부적으로 봐보자. 
'ggridges' 패키지로 ridgeline plot을 추가하겠다. 'Ridgeline plot'은 'semi-overlapping'(밀도) 곡선을 빠르게 비교할 수 있다. 장르별 일 방문자의 분포를 나태는 차트를 그리고, y축은 하나만 나타내되 두 차트 모두에서 공유한다.
```{r}
p1 <- foo %>% 
  mutate(wday = lubridate::wday(visit_date, label = TRUE)) %>% 
  group_by(wday, air_genre_name) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ggplot(aes(air_genre_name, mean_visitors, color = wday)) +
  geom_point(size = 4) +
  theme(legend.position = 'left', axis.text.y = element_blank(),
        plot.title = element_text(size = 14)) +
  coord_flip() + 
  labs(x = '', y = '평균 방문객수') +
  scale_x_discrete(position = 'top') +
  ggtitle('air_genre_name') +
  scale_color_hue()

p2 <- foo %>% 
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = 'none') +
  labs(y = '', x='방문객수') +
  scale_fill_cyclical(values = c('blue', 'red')) #장르, 요일별 차트와 햇갈려 색을 달리 했다.

layout <- matrix(c(1,1,2,2,2),1,5,byrow= TRUE)
multiplot(p1, p2, layout = layout)
```

각 색상은 요일을 나타내고, 주말은 red에 가까운 색이고, 주중은 cool톤으로 다채로운 색상이다.(월요일은 dark orange색)

결과:

- 주중과 주말의 가장 큰 차이는 'Karaoke'에서 보인다. 차이는 크지 않지만 유사한 추세로 'International'이 있다.

- 주말이 주중보다 방문객 수가 많은 추세를 뒤집는 data는 없다. 주중.주말 갭이 가장 적은 곳은 'Other', 'Japanese food'이고, 'Korean food'는 금요일이 평균 방문객이 가장 많다. 'Bar/Cocktail'은 전반적으로 방문객이 적다.

- 밀도 분포는 주간 분포에서 얻은 인상을 확인한다.: 'Asian' 레스토랑은 평균 방문객수가 10명 미만이 거의 없고, 'Karaoke'는 강한 주말의 영향으로 분포가 넓다.

## 5.2 The impact of holidays

휴일인 날과 그렇지 않은 날의 통계를 비교하여 휴일이 방문객 수에 영향을 미치는지 알아보자.
```{r}
foo <- air_visits %>% 
  mutate(calendar_date = as.character(visit_date)) %>% 
  left_join(holidays, by = 'calendar_date')

p1 <- foo %>% 
  ggplot(aes(holiday_flg, visitors, color = holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() + 
  theme(legend.position = 'none') 
  
p2 <- foo %>% 
  mutate(wday = lubridate::wday(date, label= TRUE)) %>% 
  group_by(wday, holiday_flg) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ggplot(aes(wday, mean_visitors, color = holiday_flg)) +
  geom_point(size = 4) +
  theme(legend.position = 'none') +
  labs(y = '평균 방문객 수')

layout <- matrix(c(1,2),1,2,byrow = TRUE)
multiplot(p1, p2, layout=layout)

```

결과:

- 왼쪽 차트를 보면 휴일 유무에 따른 방문객 수는 큰 차이가 없어 보인다. 숨겨진 세부 정보가 있다.

- 오른쪽 차트를 보면 휴일이 주말인 경우는 방문객 수에 큰 영향을 미치지 않고 감소한 경우도 있다. 하지만 휴일이 주중인 경우는 큰 영향을 미치는 걸로 보인다. 특히 월요일과 화요일은.

## 5.3 Restaurants per area and the effect on visitor numbers

만약 식당이 유명하거나, 그 지역에 동일한 타입의 식당이 하나뿐이라면 많은 고객을 예상할 수 있을 것 같다. 거리에 동일 타입내 12개의 다른 식당이 있다면 사람들은 여러 곳으로 갈 것이다. 경제학자들이 말하는 수요 & 공급의 법칙이다.(공급 > 수요 : 경쟁력이 낮고 , 공급 < 수요: 경쟁력이 높다.)
그러나 data set과 같은 snapshot이거나, 지역화 특성이 된 곳의 식당은 여전히 경쟁력이 있다. 그래서 지역별 특정 장르의 식당 수와 그것들이 방문객 수에 미치는 영향을 알아본다.

 'air' & 'hpg' store의 2개 data set에서 지역당 장르별 빈도수를 overview로 보겠다. 이전 장의 일부였을 수도 있지만, 확인해보겠다. 어떤 장르가 어디 영역에 존재하는지를 나타내는 차트를 만들겠다. 점의 크기는 경우의 수에 비례한다.

```{r}
air_store %>% 
  mutate(area = str_sub(air_area_name, 1, 12)) %>% 
  ggplot(aes(area, air_genre_name)) +
  geom_count(color = 'blue') +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.9))
```

결과:

- 몇 몇 지역에는 많고 다양한 레스토랑이 있지만, 어떤 지역에는 하나의 레스토랑만 있다. parameter 공간의 대부분은 비어있다.

- 유사하게, 'Izakaya', 'Cafe'와 같은 식당은 어디는 흔하지만, 어디는 그렇지 않다. 
'Hokkaido Sapporo-shi Minami 3 Jonishi'에는 'Karaoke bar'가 2군데 뿐이다. 'Tokyo-to Shibuya-ku Shibuya'에서는 'International', 'Asian' 레스토랑이 2군데씩 뿐이다.

hpg data 역시 동일하게 차트로 나타내겠다.

```{r}
hpg_store %>% 
  mutate(area = str_sub(hpg_area_name, 1, 10)) %>% 
  ggplot(aes(area, hpg_genre_name)) +
  geom_count(color = 'red') +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.9))
```

결과

- hpg 역시 air와 동일하게 분포가 높은 곳과 낮은 곳이 있다. 당연히, 도쿄는 요리가 다양하게 분포되어 있다.

- 'Japanese style'과 'Intenational' 식당은 대부분의 곳에서 인기가 있다. 'Shanghai food' or 'Dimsum'과 같이 'Amusement bars', 'Udon/Soba' 식당은 희귀하다.

이 차트들은 지역별 식당들의 장르 분포도를 보여준다. 겹치는 jitter plot이 있는 boxplot으로 세부 분포를 볼 수 있다.
장르는 지역별 분포 평균의 내림차순으로 정렬했으며, 차트에서 dot는 수평으로 나타냈다. 먼저 개별 데이터를 겹친 후에 각각의 dot에 임의 지터를 할당하여 겹치는 데이터를 구분할 것이다.y축('Occurences per area')은 점의 크기에 해당한다. 플롯이 매우 상세하기에 패널 대신 single plot을 쓰겠다.

```{r}
air_store %>% 
  group_by(air_genre_name, air_area_name) %>% 
  count() %>% 
  ggplot(aes(reorder(air_genre_name, n, FUN = mean), n)) +
  geom_boxplot() +
  geom_jitter(color = 'blue') +
  scale_y_log10() +
  coord_flip() +
  labs(x = 'Air genre', y = 'Occurences per air area')

```


결과:

- 단지 몇 몇 장르만이 지역당 2개 이상의 레스토랑 중위값을 가지고 있다. 예를 들어 'Italian/French', 'Bar/Cocktail'은 지역당 2개 이상의 그룹을 찾을 수 있다.

- 대다수 장르의 분포는 지역당 2개 정도의 클러스터에 밀집되어 있고, 'Cafes'는 한 지역에 26개의 많은 가게가 있다.

- 흥미롭게도 최소값은 1이 아니라 2다. 즉 어느 지역에도 장르를 독점한 'air' 레스토랑이 없다는 것이다.'Fig.5' 차트에서도 보듯이, 동일 지역에 2개의 레스토랑 그룹이 있으면 그들은 동일한 장르이다. 지역당 더 큰 그룹은 독점적인 장르가 나오지 않는다. 너무 완벽하게 같아서 이걸 어떻게 만들어야 할지 모르겠다. air 데이터가 선택되는 방식과 관련이 있을 수 있을까? 장르 이름 지정에 버그가 있을까? 한 지점에 있는 2 레스토랑의 몇 몇 예시를 확인하고, 그들이 같은 날 고객수가 다른걸 확인했다. 'Dining Bars'의 pair를 찾았고, 이건 단일 항목이 여기에 중복되는 문제가 없음을 나타낸다.

```{r}
air_store %>% 
  filter(air_store_id == "air_b5598d12d1b84890" | air_store_id == "air_bbe1c1a47e09f161")

air_visits %>% 
  filter(air_store_id == "air_b5598d12d1b84890" | air_store_id == "air_bbe1c1a47e09f161") %>% 
  arrange(visit_date) %>% 
  head(10)
```

'hpg'도 확인해보겠다.

```{r}

foobar <- hpg_store %>% 
  group_by(hpg_genre_name, hpg_area_name) %>% 
  count()

foobar %>% 
  ggplot(aes(reorder(hpg_genre_name, n, FUN = mean), n)) +
  geom_boxplot() +
  geom_jitter(color = 'red') +
  scale_y_log10() +
  coord_flip() +
  labs(x = 'hpg genre', y = 'Cases per hpg area')
```

결과:

- 지역당 최소 하나의 장르를 가지고 있고, 전반적으로 'air'대비 수치가 높아서 중위값이 더 다양하다.

- 흥미로운 장르는 'Japanese style'로 이건 중위값이 지역당 10개 이상이다. 이와는 반대로 지역당 최소 1개도 되지 않는 낮은 박스 힌지가 여러 장르에 있다. 

각 영역의 장르 수에 대한 정보를 바탕으로 데이터 집합의 클러스터링 또는 '복잡성'를 정량화하고 이걸 고객 수와 연관시킬 수 있다. 다음 플롯은 마지막 두 플롯(지역당 동일 장르의 사례)의 'air'및 'hpg' 데이터 포인트의 전체 분포를 보여 준다.

더해서, 각 클러스터링 사례의 평균 고객 수를 추정한다. 이를 위해 먼저 지역당 장르의 log1p로 변환된 고객 수의 평균을 구하고, 각 경우(한 지역에 동일한 장르의 발생수)에 대해 평균 및 표준편차를 계산할 것이다. 이건 방문객 수 분포를 보다 평범하게 만들기 위한 것이다. (Fig.1 차트 참조)

```{r}
foo <- air_visits %>% 
  left_join(air_store, by = 'air_store_id')

bar <- air_store %>% 
  group_by(air_genre_name, air_area_name) %>% 
  count()

foobar <- hpg_store %>% 
  group_by(hpg_genre_name, hpg_area_name) %>% 
  count()

p1 <- bar %>% 
  ggplot(aes(n)) +
  geom_histogram(fill = 'blue', binwidth = 1) +
  labs(x = 'Air genres per area')

p2 <- foobar %>% 
  ggplot(aes(n)) +
  geom_histogram(fill = 'red', binwidth = 1) +
  labs(x = 'HPG genres per area')

p3 <- foo %>% 
  group_by(air_genre_name, air_area_name) %>% 
  summarise(mean_log_visit = mean(log1p(visitors))) %>% 
  left_join(bar, by = c('air_genre_name', 'air_area_name')) %>% 
  group_by(n) %>% 
  summarise(mean_mlv = mean(mean_log_visit),
            sd_mlv = sd(mean_log_visit)) %>% 
  replace_na(list(sd_mlv = 0)) %>% 
  ggplot(aes(n, mean_mlv)) +
  geom_point(color = 'blue', size = 4) +
  geom_errorbar(aes(ymin = mean_mlv - sd_mlv, ymax = mean_mlv +sd_mlv), width = 0.5, size = 0.7, color = 'blue') +
  labs(x = 'Cases of identical Air genres per area', y = 'Mean +/- SD of\n mean log1p visitors')

layout <- matrix(c(1,2,3,3),2,2, byrow = TRUE)
multiplot(p1, p2, p3, layout = layout)
```

결과:

- 위의 차트: 동일 지역에 있는 개별 장르의 수는 'air'와 'hpg' 데이터 셋의 경우 2와 1의 최대값으로 부터 급격하게 빠질 것이다. 장르가 많은 쪽으로 꼬리가 길게 빠지는 모양이 나오고, 2번째 피크는 'air'는 8, 'hpg'는 13즈음이다.

- 아래 차트: 고객 수의 log1p는 상대적으로 크게 펼쳐진 모양이고, 2부터 9까지의 범위내에서 완벽하게 일관된다. 10은 펼쳐지지 않고 간단하다. 11과 마찬가지로 그저 2개의 포인트 밖에 없고 그렇지 않다면 이건 그저 하나의 측정치일 뿐이다. 
모든 데이터 포인트가 9 이상인 산포도가 낮은 점은 주목할 만한데, 이 거짓말 같은 수치는 9이하 포인트의 평균보다 높은 점이다. 이건 작은 지역들에 의해 높은 방문자수가 떨어지지 않은 걸 의미한다.

- 아래 차트의 산포도는 장르에 구애없이 모든 클러스터링/ 과밀 지역을 믹스해서 처리했다. 방문자수가 기록된 'air' data에 한해 차트를 그릴 수 있다.

거리에 따른 레스토랑의 특수성을 나타내는 정량화의 다른 기법은 FE할 때 다시 다루겠다.

## 5.4 Reservations Vs Visits

'air_reserve'와 'hpg_reserve' data 셋의 예약자 수를 확인해 보겠다. 4.2 & 4.3섹션에서 이들의 time-series와 분포를 봤고, 여기서는 예약자 수와 실제 방문 고객간을 비교할 것이다.

각각의 레스토랑 id와 'air_visitor'파일을 summarise해서 일별 reserve_visitors(사람들 예약 건수)의 합계를 산출할 것이다.
'hpg reservaton'을 포함하기 위해 'hpg_reserve'파일에서 'air_store' id에 해당하는 'hpg_store' id를 조인한 'store_ids'를 사용하겠다.


```{r}
foo <- air_reserve %>%

  mutate(visit_date = date(visit_datetime)) %>%

  group_by(air_store_id,visit_date) %>%

  summarise(reserve_visitors_air = sum(reserve_visitors))

  

bar <- hpg_reserve %>%

  mutate(visit_date = date(visit_datetime)) %>%

  group_by(hpg_store_id,visit_date) %>%

  summarise(reserve_visitors_hpg = sum(reserve_visitors)) %>%

  inner_join(store_ids, by = "hpg_store_id")


all_reserve <- air_visits %>%

  inner_join(foo, by = c("air_store_id", "visit_date")) %>%

  inner_join(bar, by = c("air_store_id", "visit_date")) %>%

  mutate(reserve_visitors = reserve_visitors_air + reserve_visitors_hpg)
```

'air' 레스토랑의 실제 고객수에 대한 전체 reserve_visitor를 차트로 만들겠다. ggExtra패키지의 ggMarginal 함술를 써서 marginal histogram을 포함한 산포도 차트를 만들겠다. 회색 선은 'reserve_visitors == vistors'을 나태내고, 파란 선은 linear fit이다.

```{r}
p <- all_reserve %>%

  filter(reserve_visitors < 120) %>%

  ggplot(aes(reserve_visitors, visitors)) +

  geom_point(color = "black", alpha = 0.5) +

  geom_abline(slope = 1, intercept = 0, color = "grey60") +

  geom_smooth(method = "lm", color = "blue")

ggMarginal(p, type="histogram", fill = "blue", bins=50)
```

결과:

- 히스토그램은 'reserve_visitors'와 visitor 수의 피크가 20이하에 있으며, 100이하에 범위에 국한해서 많은 분포가 몰려있음을 볼 수 있다.

- 산포도는 identity line을 크게 상회하고, 이건 예약하지 않고 방문한 고객이 더 많은걸 나타낸다. 그다지 놀랍지 않은 결과인데 대다수의 사람들이 돌아다니다 가게를 들어가기 때문이다.

- 이 포인트의 주목할 부분은 아래의 선인데, 몇 몇의 사람들은 예약을 하고도 변심해서 방문하지 않는다는 것이다. 이런 노쇼가 예상될 수 있고 이를 고려하는 것이 이 competition에서 해결해야 할 점 중 하나이다.

- 실제 방문객을 과소평가한 'reserve_visitors'의 수가 많은 이런 트렌드를 linear fit은 보여준다. 예약없이 방문하는 고객보다 더 많은 수의 예약이 캔슬되는 것을 예견할 수 있다.

- 'visitors - reserve_visitors'를 해서 'air_reserve'와 'hpg_reserve'를 구분하여 histogram으로 시각화 하겠다.
'air'(blue), 'hpg'(red)의 time-series를 -250~150사이에 offset하고, baseline은 black으로 표시하겠다.

```{r}
p1 <- all_reserve %>% 
  ggplot(aes(visitors - reserve_visitors)) +
  geom_histogram(binwidth = 5, fill = 'black') +
  coord_flip() +
  labs(x = '')

p2 <- all_reserve %>% 
  ggplot(aes(visitors - reserve_visitors_air)) +
  geom_histogram(binwidth = 5, fill = 'blue') +
  coord_flip() +
  labs(x = '')

p3 <- all_reserve %>% 
  ggplot(aes(visitors - reserve_visitors_hpg)) +
  geom_histogram(binwidth = 5, fill = 'red') +
  coord_flip() +
  labs(x = '')

p4 <- all_reserve %>% 
  ggplot(aes(visit_date, visitors - reserve_visitors)) +
  geom_hline(yintercept = c(150, 0, -250)) +
  geom_line() +
  geom_line(aes(visit_date, visitors - reserve_visitors_air +150), color = 'blue') +
  geom_line(aes(visit_date, visitors - reserve_visitors_hpg - 250), color = 'red') +
  ggtitle('Visitors - Reserved: all (black), air (blue), hpg (red)')

layout <- matrix(c(4,4,2,4,4,1,4,4,3),3,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
```

결과:

- 이 차트는 training time의 범위에서 의미있는 분산을 보여준다.'air', 'hpg' 곡선이 대부분 기준선보다 위에 있지만(예약자보다 많은 방문자), 두 데이터 셋을 결합하면 zero line에 가까운 분포의 평균을 볼 수 있다. 우측의 해당 histogram에서도 볼 수 있다.

- 섹션 4.2와 비교해서 'air'의 예약이 차이가 없음을 볼 수 있다. 여기선 hpg 예약만을 볼 수 있다.(섹션 4.3엔 차이가 없다.)
이건 같은 트렌드를 따르고, 'air' reservations를 위한 프록시로 사용될 수 있다고 가정하는게 안전함을 나타낸다.

- 우측 3개의 histogram은 대략적으로 왼쪽 패널의 해석의 편의성을 위한 time-series로 정렬된다. 이건 'reserve_visitor'의 수보다 'visitors'의 수가 더 많은 것으로 많이 왜곡되어 있음을 보여준다.포아송 분포를 써서 도보 방문객과 예약 취소자들의 분포를 섞어서 보여준다.

마지막으로, 예약자와 방문객 사이의 불일와 공휴일 간의 영향을 보겠다. 

```{r}
all_reserve %>% 
  mutate(date = visit_date) %>% 
  left_join(holidays, by = 'date') %>% 
  ggplot(aes(visitors - reserve_visitors, fill = holiday_flg)) +
  geom_density(alpha = 0.5)
```

결과:

- 공휴일엔 예약후 방문객보다 바로 방문하는 사람들이 많음을 볼 수 있다. 피크는 거의 동일하지만, 큰 수에 대해서는 작지만 분명한 차이를 볼 수 있다.

[원문]("https://www.kaggle.com/headsortails/be-my-guest-recruit-restaurant-eda/report")