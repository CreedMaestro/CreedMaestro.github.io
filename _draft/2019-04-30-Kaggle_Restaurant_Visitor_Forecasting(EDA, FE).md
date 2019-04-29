---
title: "캐글 Restaurant Visitor Forecasting Introduction, EDA"
author: "Maestro"
date: "2019/04/25"
categories: Kaggle
tags: Kaggle, 캐글, 캐글코리아, 고객예측
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

```{r split=FALSE, fig.align='default', warning=FALSE, fig.cap='Fig. 15', out.width='100%'}
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

- 