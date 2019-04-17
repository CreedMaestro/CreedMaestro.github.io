---
title: "캐글코리아 2nd Competition House Price Prediction with R (시애틀 집값 예측) Introduction ~ EDA"
author: "Maestro"
date: "2019/04/10"
categories: Kaggle
tags: Kaggle, 캐글, 캐글코리아
layout: post
output: 
  md_document:
    variant: markdown_github
---


<br>

# 1 Introduction

<br>

 설명과 첨언의 글이 많아 이후 경어는 생략하고 진행하겠습니다.

 [원문 링크] ( md 파일이라 rmd 문법이 맞지 않아 수정해서 올리니, 캐글로 들어온 분들이라면 원문 링크로 들어가서 확인 바란다.)
   (수정중)
 
 참조 
 
 1. https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/report
 
 2. https://www.kaggle.com/chocozzz/house-price-prediction-eda-updated-2019-03-12
 
 3. https://www.kaggle.com/psystat/house-price-prediction-eda-and-lasso-with-r

  
 
<br>

 캐글코리아 주최 2회째 Competition으로 주제는 House price prediction이며, 캐글에 있는 집값 예측은 보스턴 지역이지만, 캐코는 시애틀 지역이다.
분석 언어는 'R'을 쓰며, 주 패키지는 분석용 dplyr와 시각화 ggplot2이며, 이 두 패키지가 포함된 
tidyverse를 쓸 예정이다.
데이터 확인부터 모델링까지 진행 예정이며, 현재 작업중인 자료라 차근차근 올릴 계획이다.
첫 Competition이기에 다른 분들의 자료를 참고하여 만들었으며, 참조한 커널은 링크를 첨부한다.

---

# 2 Loading library & data

## 2.1 Load libraries

<br>
```{r}
library(tidyverse)   # ggplot2, dplyr, tidyr, tibble, readr 외 다양한 패키지가 있는 복합 패키지
library(corrplot)    # 시각화 사용(상관 계수 파악)
library(data.table)  # data Load
library(plotly)      # price 변수 인터랙티브 그래프 확인
library(scales)      # 그래프 축 수치 변경
library(gridExtra)   # 차트 멀티 표현
library(Rmisc)       # multiplot
library(zipcode)     # zipcode to city 변환
library(ggiraphExtra)# 지도 매핑 & 시각화
library(psych)       # Skewness
library(caret)       # preProcess 
```

<br>
## 2.2 Load Data
```{r}
# 데이터는 [competition 데이터셋]("https://www.kaggle.com/c/2019-2nd-ml-month-with-kakr/data") 에서 받을 수 있다.
k_train <- fread("./kako 2nd/train.csv")
k_test  <- fread("./kako 2nd/test.csv")
```

---

# 3 Data Structure and Description

## 3.1 Data Structure

```{r}
dim(k_train)  # 21개 변수의 15035 관측치다.
dim(k_test)   # price를 제외한 20개 변수의 6468 관측치이며, k_train대비 비율은 5:2이다.
str(k_train)  # 숫자 & 문자 변수 혼합 타입으로 구성된 2차원 이상의 데이터이다.
str(k_test)   # 숫자 & 문자 변수 혼합 타입으로 구성된 2차원 이상의 데이터이다.
```

---

## 3.2 Combined the datasets

```{r}
k_test.labels <- k_test$id # modeling 대비 id 벡터에 저장
k_all <- bind_rows(within(k_train, rm('id')), within(k_test, rm('id')))
# rbind는 열 개수가 다르면 결합 불가. bind_rows는 없던 열 결측치로 대체하여 결합.
dim(k_all)
str(k_all)
```

## 3.3 Data Description


+ ID : 집을 구분하는 번호
+ date : 집을 구매한 날짜
+ price : 집의 가격(Target variable)
+ bedrooms : 침실의 수
+ bathrooms : 화장실의 수 [(참고)](https://www.kaggle.com/c/2019-2nd-ml-month-with-kakr/discussion/83533)
+ sqft_living : 주거 공간의 평방 피트(면적)
+ sqft_lot : 부지의 평방 피트(면적)
+ floors : 집의 층 수
+ waterfront : 집의 전방에 강이 흐르는지 유무 (a.k.a. 리버뷰)
+ view : 집이 얼마나 좋아 보이는지의 정도
+ condition : 집의 전반적인 상태
+ grade : King County grading 시스템 기준으로 매긴 집의 등급(구조 & 디자인)
( 하등급: 1~3등급, 중등급: 7 등급, 상등급: 11~13 등급 )
[(참고)](https://www.kaggle.com/c/2019-2nd-ml-month-with-kakr/discussion/83536)
+ sqft_above : 지하실을 제외한 평방 피트(면적)
+ sqft_basement : 지하실의 평방 피트(면적)
+ yr_built : 지어진 년도
+ yr_renovated : 집을 재건축한 년도
+ zipcode : 우편번호
+ lat : 위도
+ long : 경도
+ sqft_living15 :  2015년 기준 주거 공간의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음) 
+ sqft_lot15 :  2015년 기준 부지의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음) 

---

# 4 EDA(Exploratory Data Analysis)

<br>

## 4.1 Missing Values

```{r}
sum(is.na(k_all))/(ncol(k_all)*nrow(k_all))  # train, test 결합하며 생긴 price의 결측치 같다.
colSums(is.na(k_all))  #row 결합으로 생긴 price의 결측치만 보인다.
# study 개념으로 시각화를 첨부한다.
# 각 feature 결측치 비율 계산
missing_values <- k_all %>% dplyr::summarise_all(funs(sum(is.na(.))/n()))
# data frame 생성
missing_values <- tidyr::gather(missing_values, key = 'feature', value = 'missing_pct')
# 그래프 생성
missing_values %>% 
  ggplot(aes(x= reorder(feature, missing_pct), y = missing_pct)) + # missing_pct 내림차순 정렬
  geom_bar(stat = 'identity', fill = 'red') + # 막대 그래프 생성
  ggtitle('Rechecking missing values in each features') + # 제목 
  theme(plot.title = element_text(face = 'italic',        # 글씨체
                                  hjust = 0.5,            # 가로비율(Horizon)
                                  size = 15,              # 폰트 크기
                                  color = 'black')) +     # 폰트 컬러
labs(x = 'Feature names', y = 'Rate') +                   # x, y 축 명명
  coord_flip()                                            # x, y 축 반전
# 시각화 확인 결과 데이터셋 합산하며 생긴 price 제외 NA는 없다.
```

<br>

## 4.2 Data detail overview

```{r}
head(k_all,10)  # Chapter 3.3 설명대로 값들은 예상과 맞지만, date는 수정할 필요가 있어 보인다.
summary(k_all)  # 수치형이지만 실제론 범주형인 값들이 보인다.
```

<br>

## 4.3 Make a fuction for plotting actually categoric data

```{r}
plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(k_all[,col]), decreasing = TRUE))
  
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
  
    print(num.plot)
  }
}
```

<br>

## 4.4 Modify a date format

```{r}
k_all[, date:=substr(date, 1, 8)]   # date에서 불필요 부분 제거하여 연간 구매로 변수 생성
k_all %>% select(date) %>% head(10)
```

<br>

## 4.5 detail price & visualization

```{r}
summary(k_all$price)  # price 분포 확인
price.g <- ggplot(data=k_all[!is.na(k_all$price)], aes(x=price)) + 
  geom_histogram(fill = 'red', binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma)
ggplotly(price.g)     # 인터랙티브 그래프로 금액대별 거래량 분포 확인
```

 금액대는 35만에서 거래가 가장 많았으며, 평균은 54만이고, 75% 거래량 까지가 64만으로 
 주로 100만 안에서 거래량이 많았다. 200만이 넘어서는 10건 이하로 거래가 되었고, 
 4백만 이후부터는 1건씩 거래가 이루어졌다.
 
<br>

## 4.6 Correlation with price
 상관 관계 히트맵
 
```{r} 
str(k_all)  # pearson(수치형) & spearman(수치 & 범주형 혼합) 계수 생성위해 확인
k_numericVars <- which(sapply(k_all, is.numeric))  # index vector numeric variables
k_numericVarnames <- names(k_numericVars) # 이름 명명
cat('There are', length(k_numericVars), 'numeric variables')

k_all.numVar <- k_all[, ..k_numericVars]
k_cor.numVar <- cor(k_all.numVar, use = 'pairwise.complete.obs', method="spearman") # 결측값 제외 상관행렬 생성
# price 기준 내림차순 정렬
k_cor.sorted <- as.matrix(sort(k_cor.numVar[, 'price'], decreasing = TRUE)) 
k_corHigh <- names(which(apply(k_cor.sorted, 1, function(x) abs(x) > 0)))
k_cor.numVar <- k_cor.numVar[k_corHigh, k_corHigh] 
# 상관 행렬 히트맵
corrplot.mixed(k_cor.numVar, 
               tl.col = 'black',  # 변수명 색깔
               tl.pos = 'lt',     # 변수명 왼쪽 표시
               tl.cex = 0.7,      # 변수명 text 크기
               cl.cex = 0.7,      # y축 상관계수 text 크기
               number.cex = .5    # matrix안 상관계수 text 크기
               )     
```

<br>

 상관 행렬 확인 결과 주거 공간의 크기, 등급, 지상 평방피트 등 상위에
 공간의 면적과 관련된 항목들이 있음을 알 수 있다. 그리고 침실의 개수보단 욕실이 가격과
 상관 관계가 더 높고, 한국이라면 영향이 컸을 리버뷰(waterfront)는 상관 관계가 그리 높지 않았다.
 의문은 위도와는 상관 관계가 0.45이지만, 경도와는 0.07로 이건 지역의 특성인것 같은데 확인해 봐야 할 것 같다.

<br>

## 4.7 Label Encoding / Factorizing variables


<br>

### 4.7.1 grade variables
```{r}
# grade boxplot (하등급: 1~3등급, 중등급: 7 등급, 상등급: 11~13 등급)
k_g1 <- ggplot(data=k_all[!is.na(k_all$price),], aes(x= factor(grade), y = price)) + #grade 범주형 변수로 변환
  geom_boxplot(col = 'black') + labs(x = 'grade') + 
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$grade[!is.na(k_all$price)]== 11 & k_all$price[!is.na(k_all$price)] > 7000000, rownames(k_all),''), vjust = 1.5))

k_g2 <- ggplot(data = k_all, aes(x = as.factor(grade))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size =3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_g1, k_g2)
```

<br>

11등급인데 가격이 너무 높은 이상치가 있다. 확인해 봐야 할 것 같다.

<br>

### 4.7.2 sqft_living variables

```{r}
# sqft_living 분포 확인
ggplot(data=k_all[!is.na(k_all$price)], aes(x = sqft_living, y=price)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se= FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$sqft_living[!is.na(k_all$price)] > 10000, rownames(k_all), ''), vjust=1.5))
```

<br>
price 관측치 '8913'은 면적 대비 가격이 낮아 보인다. 가장 상위 가격인 '5109'와 비교해보겠다.
```{r}
k_all[c(5109, 8913), c('price','sqft_living', 'grade','bathrooms', 'bedrooms')]
```
면적, 등급, 욕실 상태와 침실 수도 큰 차이는 안나지만 가격이 3배 차이 나는건 확인해 봐야겠다.

<br>

### 4.7.3 sqft_living15 variables
```{r}
# sqft_living15 분포 확인
ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_living15, y = price)) + 
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) + 
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(sqft_living15[!is.na(k_all$price)] > 6000, rownames(k_all), ''), vjust = 1.5))
```

<br>

living15에서도 면적 대비 가격이 낮은 값들이 보인다. living에선 이상치가 2건 정도였는데,
여기선 꽤 많은 값들이 있다. 여기선 변별하기가 힘들것 같아 추후 다른 값에서 비교해보겠다.

<br>

### 4.7.4 sqft_above variables
```{r}
# sqft_above 분포 확인
ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_above, y = price)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) + 
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$sqft_above[!is.na(k_all$price)] > 7500, rownames(k_all), ''), vjust= 1.5))
```

<br>

### 4.7.5 bathrooms variables
```{r}
# bathrooms boxplot(0.5:세면대 화장실, 0.75: 0.5+샤워실, 1: 0.75+욕조)
k_b1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = factor(bathrooms), y = price)) + #bathrooms 범주형 변수로 변환
  geom_boxplot( col = 'black') +labs(x = 'bathrooms') +
  scale_y_continuous(breaks = seq(0,8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(bathrooms[!is.na(k_all$price)]<=5.25 & price>5000000, rownames(k_all), ''), vjust = 1.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

k_b2 <- ggplot(data = k_all, aes(x = as.factor(bathrooms))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_b1, k_b2)
```

<br>

4.25부터 5.5까지 이상치가 존재하고, 5.5와 5.75는 최대치가 높아보인다. 
확인해 봐야 할 것 같다.

<br>

### 4.7.6 lat variables
```{r}
# lat 
ggplot(data = k_all[!is.na(k_all$price),], aes(x = lat, y = price)) + 
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), label = comma) +
  geom_text(aes(label = ifelse(price > 6000000, rownames(k_all), ''), vjust = 1.5))
```

<br>

위도가 상승할수록 가격도 오르는 걸 확인할 수 있다. 추후에 연관성이 있는 항목들을
재검토 하겠다.

<br>

### 4.7.7 bedrooms variables
```{r}
# bedrooms boxplot
k_d1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = factor(bedrooms), y = price)) + # bedrooms 범주형 변수로 변환
  geom_boxplot(col = 'black') + labs(x = 'bedrooms') + 
  scale_y_continuous(breaks = seq(0,8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(price > 6000000, rownames(k_all), ''), vjust = 1.5))

k_d2 <- ggplot(data = k_all, aes(x = as.factor(bedrooms))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_d1, k_d2)
```

<br>

3,4,5,6에서 이상치가 많이 보이고, 5,6은 특출난 수치들이 있다. 확인해 봐야 할 것 같다.

<br>

### 4.7.8 floors variables

```{r}
k_f1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = factor(floors), y = price)) +
  geom_boxplot(col = 'black') + labs(x = 'floors') +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$floors[!is.na(k_all$price)]==2.5 & price > 7000000, rownames(k_all), ''), vjust = 1.5))

k_f2 <- ggplot(data= k_all, aes(x = as.factor(floors))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_f1, k_f2)
```

'5109'는 floors 변수에서도 이상치로 나타난다. 면적대비 가격이 낮으면서 2.5층의 값이다.

<br>

### 4.7.9 view variables
```{r}
# view boxplot
k_v1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = factor(view), y = price)) + # view 범주형 변수로 변환
  geom_boxplot(col = 'black') + labs(x = 'view') +
  scale_y_continuous(breaks = seq(0,8000000, by = 1000000), label = comma) +
  geom_text(aes(label = ifelse(price > 5500000, rownames(k_all), ''), vjust = 1.5))

k_v2 <- ggplot(data = k_all, aes(x = as.factor(view))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_v1, k_v2)
```

<br>

0,2,3,4가 view 대비 특출난 이상치들이 있다. 확인해 봐야 할 것 같다.

<br>

### 4.7.10 sqft_basement variables
```{r}
# sqft_basement 분포 확인
ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_basement, y = price)) + 
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0,8000000, by = 1000000), label = comma) +
  geom_text(aes(label = ifelse(price > 6000000, rownames(k_all), ''), vjust = 1.5))
``` 

<br>

지하실 면적과 가격간의 상관은 있어보이지만, 0에 많은 수치가 몰려있다.
지하실이 없을 경우를 0으로 표현한 것 같은데, 확인해 봐야 할 것 같다.

<br>

### 4.7.11 waterfront variables

```{r}
k_w1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x= factor(waterfront), y = price)) +
  geom_boxplot(col = 'black') + labs(x = 'waterfront') +
  scale_y_continuous(breaks = seq(0,8000000, by = 1000000), labels = comma)

k_w2 <- ggplot(data = k_all, aes(x = as.factor(waterfront))) +
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(k_w1, k_w2)
```

강이 보이는 곳이 평균적으로 가격이 높지만, 강이 안 보이더라도 비싼 곳들이 있다.

### 4.7.12 yr_renovated variables

```{r}
ggplot(data=k_all[!is.na(k_all$price),], aes(x = factor(yr_renovated), y = price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) 
k_all %>% select(yr_renovated) %>% table()
```

값이 0인 값들이 많다. 재건축 유무를 구분하여 변수를 새로 만들어서 확인해봐야 할 것 같다.
년도는 테이블로 확인하였다.

<br>

### 4.7.13 yr_built variables

```{r}
ggplot(data=k_all[!is.na(k_all$price),], aes(x = factor(yr_built), y =price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se= FALSE, color ='black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))
k_all %>% select(yr_built) %>% table()
```

집값과의 상관 관계는 크게 없어 보인다.
차트에선 년도가 잘 안보여 수치로 보았다. 1900년~ 2015년까지 분포되어 있다.

<br>

### 4.7.14 sqft_lot variables

```{r}
ggplot(data=k_all[!is.na(k_all$price),], aes(x = sqft_lot, y = price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group=1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$sqft_lot[!is.na(k_all$price)] > 1500000, rownames(k_all), ''), vjust= 1.5))
summary(k_all$sqft_lot)
```

상관 관계는 보이지만, 75%의 수가 0~1만 사이에 몰려있다. Min과 Max의 차가 굉장히 크다.

<br>

### 4.7.15 sqft_lot15

```{r}
ggplot(data=k_all[!is.na(k_all$price),], aes(x = sqft_lot15, y = price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma)
```

<br>

### 4.7.16 long variables

```{r}
ggplot(data = k_all[!is.na(k_all$price),], aes(x = long, y = price)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 10000000), labels = comma)
```

lat과는 달리 long은 큰 상관 관계가 보이지 않는다.

<br>

### 4.7.17 condition variables

```{r}
k_c1 <- ggplot(data= k_all[!is.na(k_all$price),], aes(x = factor(condition), y= price)) +
  geom_boxplot(col= 'black') + labs(x = 'condition') +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma) +
  geom_text(aes(label = ifelse(k_all$condition[!is.na(k_all$price)] >= 3 & price > 6000000, rownames(k_all), ''), vjust = 0.5, hjust=1.5))

k_c2 <- ggplot(k_all, aes(x = as.factor(condition))) + 
  geom_histogram(stat = 'count') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust =1))
grid.arrange(k_c1, k_c2)
```

<br>

### 4.7.18 zipcode variables

```{r}
k_z1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(zipcode), y = price)) +
  geom_point(color = 'blue') +
  geom_smooth(methdo = 'lm', se= FALSE, color = 'black', aes(group = 1)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 10000000), labels = comma)

k_z2 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(zipcode), group= factor(zipcode), y = price)) +
  geom_boxplot(color = 'black') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 8000000, by = 1000000), labels = comma)
grid.arrange(k_z1, k_z2)
```

zipcode는 추후 다시 살펴봐야겠다.

<br>

## 4.8 high correlations related variables visualization

### 4.8.1 sqft_living, and other surface related variables (in square feet)

섹션 4.6에서 살펴본 바와 같이 상관 계수 상위에 면적과 관련한 변수가 많이 분포되어 있다.
실제적으로 집을 구매시 집의 면적은 가격과 더불어 아주 중요하다. 
여기서는 면적 관련한 변수들을 한눈에 확인하여 또 다른 인사이트를 얻을 수 있을지 알아보겠다.
욕실과 침실은 면적에는 포함되지 않지만, living_area와 상관 관계가 높기(0.75, 0.65)에 추가를 하였다.

```{r}
k_s1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_living)) +
  geom_density() + labs(x= 'Square feet living area')
k_s2 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_living15)) +
  geom_density() + labs(x='Square feet living area(year:2015)')
k_s3 <- ggplot(data = k_all[!is.na(k_all$price)& k_all$sqft_lot<100000], aes(x = sqft_lot)) +
  geom_density() + labs(x = 'Square feet lot') # 시각화 위해 10만 이상의 값 제거
k_s4 <- ggplot(data = k_all[!is.na(k_all$price)& k_all$sqft_lot15<100000], aes(x = sqft_lot15)) +
  geom_density() + labs(x = 'Square feet lot(year:2015)') # 시각화 위해 10만 이상의 값 제거
k_s5 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_above)) +
  geom_density() + labs(x = 'Square feet above')
k_s6 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = sqft_basement)) +
  geom_density() + labs(x = 'Square feet basement')
k_s7 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(bedrooms))) +
  geom_histogram(stat = 'count') + labs(x = 'Bedrooms')
k_s8 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(bathrooms))) +
  geom_histogram(stat = 'count') +labs(x = 'Bathrooms') +
  theme(axis.text.x = element_text(angle =90, hjust = 1))

k_layout <- matrix(c(1,3,2,4,5,6,7,8),4,2, byrow = TRUE) # 4행 2열 ()안의 순으로 행부터 matrix 생성
multiplot(k_s1, k_s2, k_s3, k_s4, k_s5, k_s6, k_s7, k_s8, layout = k_layout)
```

면적은 전반적으로 왼쪽으로 많이 치우친 모양이지만, 앞에서도 확인했듯이 몇 몇 이상치가 수치를 끌어올리는 것으로 보인다.
이상치는 추후에 확인 후 제거하겠다.

<br>

### 4.8.2 grade, and other Quality variables

집의 등급과 전반적인 상태를 나타내는 변수가 3개 있어 같이 시각화 해보겠다.

```{r}
k_q1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(grade))) +
  geom_histogram(stat = 'count') + labs(x = 'Grade')
k_q2 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(condition))) +
  geom_histogram(stat = 'count') + labs(x = 'Condition')
k_q3 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = as.factor(view))) +
  geom_histogram(stat = 'count') +labs(x = 'View')

k_layout <- matrix(c(1,2,3), 1,3, byrow = TRUE)
multiplot(k_q1, k_q2, k_q3, layout = k_layout)
```

'grade'변수는 7,8에 다수가 몰려 있으며, 'condition' 변수는 3에, 'View'는 0에 다수가 몰려 있다.
'View'가 0에 많은 변수가 몰려 있는게 기준이 어떻게 되는지 궁금하다.

<br>

### 4.8.3 Seperated date variables

연별, 월별 구매량을 date변수를 분할하여 알아보겠다.

```{r}
k_all[, Yrbuy:=substr(date, 1,4)]    # 연간 구매 확인 위해 변수 생성
k_all[, Mobuy:=substr(date, 5, 6)]   # 월간 구매 확인 위해 변수 생성 

k_y1 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = Yrbuy, y = price)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  geom_label(stat = 'count', aes(label= ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 600000)) + 
  geom_hline(yintercept = 450000, linetype ='dashed', color = 'red') + #price 중위
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  theme_light()+ theme(axis.text.x = element_text(vjust = 0.5)) + labs(x = 'Year median') 

k_y2 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = Mobuy, y = price)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size=2) +
  coord_cartesian(ylim = c(0, 600000)) + 
  geom_hline(yintercept = 450000, linetype = 'dashed', color = 'red') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  theme_light() + theme(axis.text.x = element_text(vjust=0.5)) + labs(x = 'Month median')

k_y3 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = Yrbuy, y = price)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 600000)) + 
  geom_hline(yintercept = 540683, linetype='dashed', color = 'red') + #price 평균값
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  theme_light() + theme(axis.text.x = element_text(vjust = 0.5)) + labs(x = 'Year mean')

k_y4 <- ggplot(data = k_all[!is.na(k_all$price),], aes(x = Mobuy, y = price))+
  geom_bar(stat = 'summary', fun.y = 'mean', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size=2) +
  coord_cartesian(ylim = c(0, 600000)) + 
  geom_hline(yintercept = 540683, linetype = 'dashed', color = 'red') + #price 평균값
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  theme_light() + theme(axis.text.x = element_text(vjust = 0.5)) + labs(x = 'Month mean')

k_layout <- matrix(c(1,2,3,4),2,2, byrow = TRUE)
multiplot(k_y1, k_y2, k_y3, k_y4, layout = k_layout)
```


2015년 대비 2014년의 구매 빈도가 2배 이상 높고, 월별 구매는 1,2월에는 다른 월 대비 구매 빈도가 낮다.
구매가의 중위값으로 보면 유의미한 결과는 따로 없어 보인다.

---

FE와 모델링은 추후에 업로드 예정

[캐글 RMD Report 링크]
("수정중")