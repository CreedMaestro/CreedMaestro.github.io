---
title: "캐글코리아 2nd Competition House Price Prediction with R (시애틀 집값 예측) FE"
author: "Maestro"
date: "2019/04/10"
categories: Project
tags: Kaggle, 캐글, 캐글코리아, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github
---

# 5 Feature Engineering

건축년도로 구분한 집의 년수와, 리모델링 여부, 신축 여부를 구분 지어 변수를 만들겠다.

## 5.1 Adding 'House Age', Remodeled(Yes/No)', IsNew

```{r}
k_all$Remod <- ifelse(k_all$yr_built > k_all$yr_renovated, 0, 1) # 0 = '리모델링 X', 1 = '리모델링'
temp <- ifelse(k_all$yr_renovated==0, k_all$yr_built, k_all$yr_renovated) #재건축이 아니면 디폴트로 건축년도 설정
k_all$Age <- as.numeric(k_all$Yrbuy)-temp

ggplot(k_all[!is.na(k_all$price),], aes(x = Age, y=price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label=ifelse(price > 7000000, rownames(k_all), ''), vjust = 0.5))
```

재건축을 하거나, 건축된지 30년 미만의 집이 고가에 거래된 경우가 많다.

```{r}
ggplot(k_all[!is.na(k_all$price),], aes(x = as.factor(Remod), y = price)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 6) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  theme_grey(base_size = 18)+
  geom_hline(yintercept = 450000, linetype = 'dashed')
```

재건축된 집이 더 비싸다.

```{r}
k_all$IsNew <- ifelse(k_all$yr_renovated>=2014, 1, 0) # 1은 신축, 0은 기
table(k_all$IsNew)
```
<br>
107채의 14년 이후 신축집은 train과 test셋에 분배되어 있으며, 기존 집들보다 더 비싼 경향을 보인다.

```{r}
ggplot(k_all[!is.na(k_all$price),], aes(x = as.factor(IsNew), y = price)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 6) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  theme_light() +
  geom_hline(yintercept = 450000, linetype='dashed') # 점선은 판매가의 중위값

```

## 5.2 Adding 'city' variables
zipcode 라이브러리를 써서 zipcode별 city를 변수로 추가하겠다.
```{r}
zc <- data.frame(k_all$zipcode) # zipcode 변수 추출
data(zipcode) # zipcode 라이브러리에서 변수 생성
colnames(zc)[1]='zip' # join 사전작업 컬럼명 동일하게 변경
zipcode$zip=as.integer(zipcode$zip) # join 사전작업 character > int로 변환
zc = left_join(zc,zipcode,by='zip') # left_join으로 'zc' 기준 결합
zc = zc %>% select(-'state') # 모든 state가 'WA'로 동일하므로 state 컬럼 삭제
k_all <- mutate(k_all, city= zc$city) # city 변수 추가

```
<br>
city별 가격 분포 확인
```{r}
ggplot(data = k_all[!is.na(k_all$price),], aes(x = city, group= city, y = price)) +
  geom_boxplot(color = 'black') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(labels = comma)

ggplot(k_all[!is.na(k_all$price),], aes(x = city)) + 
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))

```
<br>
'Seattle'과 'Bellevue'는 분포가 많고, 이상치들로 가격대가 높은 집들이 좀 있어 보이고, 
'Medina'는 분포 대비 부촌으로 보인다.(36채로 가장 적은 분포)

```{r}
k_zc1 <- ggplot(k_all[!is.na(k_all$price),], aes(x = reorder(city, price, FUN = median), y = price))+
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  labs(x = 'city', y = 'Median_price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 2.5) +
  geom_hline(yintercept = 450000, linetype = 'dashed', color = 'red')
  

k_zc2 <- ggplot(k_all[!is.na(k_all$price),], aes(x = reorder(city, price, FUN = mean), y = price))+
  geom_bar(stat = 'summary', fun.y = 'mean', fill = 'blue') +
  labs(x = 'city', y = 'Mean_price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 2.5) +
  geom_hline(yintercept = 450000, linetype = 'dashed', color = 'red')

grid.arrange(k_zc1, k_zc2)
```
<br>
평균값과 중위값은 'Medina'가 가장 높고, 그 다음이 지도상으로 그 바로 아래 위치한 'Mercer Island'이다.
이 변수에서 위도와 가격과의 상관 관계가 높게 나왔을 것으로 추측된다.(경도는 비슷, 위도가 약간 아래)
분포가 많았던 'Seattle'은 분포와 가격과의 상관은 크게 없어 보인다.

```{r}
k_zc3 <- ggplot(k_all[!is.na(k_all$price),], aes(x = reorder(city, price, FUN = max), y = price))+
  geom_bar(stat = 'summary', fun.y = 'max', fill = 'blue') +
  labs(x = 'city', y = 'Max_price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 2.5) +
  geom_hline(yintercept = 450000, linetype = 'dashed', color = 'red')


k_zc4 <- ggplot(k_all[!is.na(k_all$price),], aes(x = reorder(city, price, FUN = min), y = price))+
  geom_bar(stat = 'summary', fun.y = 'min', fill = 'blue') +
  labs(x = 'city', y = 'Min_price') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 2.5) +
  geom_hline(yintercept = 450000, linetype = 'dashed', color = 'red')

grid.arrange(k_zc3, k_zc4)
```
<br>
최대값을 보면 처음에 봤던 박스 플롯의 분포를 확인할 수 있고, 최소값을 보면 'Medina'와 'Mercer Island'는 
타 지역대비 높은 값을 보이는 걸 알 수 있다.
상기의 도시별 차트들을 보면 시애틀과 벨뷰에 많은 인구가 살고, 메디나와 머서 아일랜드에는 높은 가격대의 집들이 있어 이들 일대가 시가지이고, 이 곳에서 멀어질수록 가격도 낮아지고, 분포도 적은 것을 알 수 있다.

## 5.3 Adding 'Expensive city' variables

'Medina'와 'Mercer Island'는 가격과  상관도가 높은 변수이다.
그래서 이 둘을 묶은 변수를 하나 추가하겠다.
```{r}
k_all$mecity <- ifelse(k_all$city=='Medina' | k_all$city=='Mercer Island', 1, 0) # 1은 부촌, 0은 그 외
```

## 5.4 Adding 'sqft_lot per price' & 'sqft_living per price' variables

부지 기준 평당 단가와 건물 기준 평당 단가 변수 생성 (수정중)
```{r}
livingp <- k_all[!is.na(k_all$price),] %>% group_by(zipcode) %>% summarise(mean_living_per_price=mean(price/sqft_living), mean_lot_per_price=mean(price/sqft_lot))
#k_all <- left_join(k_all, livingp, by = 'zipcode') %>% as.data.table
#k_all$living_per_price <- k_all$price/k_all$sqft_living
#k_all$lot_per_price <- k_all$price/k_all$sqft_lot
str(livingp)
```

도시별 가격을 비교했을 때 봤던 것과 같이 건물당 단가는 'Medina', 'Mercer Island', 'Bellevue' 순으로 동일하다.
하지만 부지당 단가로 비교하면 건물당 단가일때는 4,9번째였던 'Seattle'과 'Issaquah'가 1,3번째로 높게 나온다.
이들 두 곳은 건물 대비해서 부지가 넓어 보인다.
'Medina'와 'Mercer Island','Belleuvue'는 부지보단 건물로 인해 가격이 높게 나오는 것 같다.

## 5.5 Adding 'sqft_living / sqft_lot' variables
상기 부지당 평단가와 건물당 평단가를 비교한 상기의 변수에서 좀 더 확인하기 위해 추가했다.

```{r}
k_all$living_divide_lot <- k_all$sqft_living/k_all$sqft_lot

ggplot(k_all[!is.na(k_all$price),], aes(x = reorder(city, living_divide_lot, FUN = mean), y = living_divide_lot))+
  geom_bar(stat = 'summary', fun.y = 'mean', fill = 'blue') +
  labs(x = 'city', y = 'living_divide_lot') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 5, by = 1),labels = comma) 
```

## 5.6 Adding 'TotalRooms' variables
'bathrooms' + 'bedrooms'해서 'TotalRooms' 변수를 만들겠다.
```{r}
k_all$TotalRooms <- k_all$bathrooms + k_all$bedrooms
```


<br>
부지당 평단가가 높았던'Issaquah'와 'Seattle'이 주거 공간 대비 부지의 공간이 넓다. (수정중)
```{r}
#living15 변수 의미 파악 위한 작업

#living15 <- k_all[!is.na(k_all$price),] %>% filter(city == 'Medina') %>% select(lat, long, sqft_living, sqft_living15, sqft_lot, sqft_lot15, living_divide_lot) %>% arrange(desc(lat))

#Medina <- k_all[!is.na(k_all$price),] %>% filter(city == 'Medina') %>%  arrange(desc(lat))

#k_all[!is.na(k_all$price),] %>% filter(city == 'Medina' , (lat >= 47.63 & lat< 47.64)) %>% select(lat, long, sqft_living, sqft_living15, sqft_lot, sqft_lot15) %>% arrange(desc(lat))
```


## 5.7 Again Correalation with price

FE 끝난 시점에서 상관 관계를 다시 확인해 보겠다.
```{r}
k_all3 <- as.data.table(k_all) # 상관 행렬위해 임시 data.table 생성
k_again.numericVars <- which(sapply(k_all3, is.numeric))
k_again.numericVarnames <- names(k_again.numericVars)
cat('there are', length(k_again.numericVars), 'numeric variables')
```

```{r}
k_again.all.numVar <- k_all3[, ..k_again.numericVars]
k_again.cor.numVar <- cor(k_again.all.numVar, use = 'pairwise.complete.obs', method = 'spearman')

k_again.cor.sorted <- as.matrix(sort(k_again.cor.numVar[, 'price'], decreasing = TRUE))
k_again.corHigh <- names(which(apply(k_again.cor.sorted, 1, function(x) abs(x) > 0)))
k_again.cor.numVar <- k_again.cor.numVar[k_again.corHigh, k_again.corHigh]
corrplot.mixed(k_again.cor.numVar,
               tl.col = 'black', 
               tl.pos = 'lt',
               tl.cex = 0.7,
               cl.cex = 0.7,
               number.cex = .5)
```

[캐글 RMD Report 링크]
("https://www.kaggle.com/maestroyi/house-price-prediction-with-r-eda-fe/report")