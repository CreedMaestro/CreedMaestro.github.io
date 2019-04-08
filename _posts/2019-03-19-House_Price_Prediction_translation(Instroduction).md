---
title: "House Prices Prediction with R (한글 번역) Introduction ~ 데이터 구조, 대표 변수 확인"
author: "Maestro"
date: "2019/03/19"
categories: Kaggle
tags: Kaggle
layout: post
output: 
  md_document:
    variant: markdown_github
    
---



***

#1 Introduction

 kaggle의 'House price prediction' 대회이며, R 커널 번역 & 의역 필사본이다.
역자 첨언은 "참조), 첨)"으로 표시하였다.
상기 필사본은 Lasso, XGBoost를 앙상블해서 예측을 했으며, 세부적인 EDA가 특징이다.
원문 링크는 아래에 첨부하며, 필사는 Rmd파일이지만, 깃헙 블로그 특성상 md 파일만 올라가기에 가독성을 위해 약간의 수정을 했다.
Rstudio상으로 돌려보길 원하시는 분들은 아래에 커널 링크를 달아둘테니 거기서 확인바란다.
코드가 너무 길어 3편으로 분할하여 올린다.
1. Introduction ~ 데이터 구조, 대표 변수 확인
2. EDA, FE
3. Modeling
***

#2 Executive Summary

I started this competition by just focusing on getting a good understanding of the dataset. 
The EDA is detailed and many visualizations are included. This version also includes modeling. 

* Lasso regressions performs best with a cross validation RMSE-score of 0.1121. 
Given the fact that there is a lot of multicolinearity among the variables, this was expected. 
Lasso does not select a substantial number of the available variables in its model, as it is supposed to do.
* The XGBoost model also performs very well with a cross validation RMSE of 0.1162. 
* As those two algorithms are very different, averaging predictions is likely to improve the predictions. 
As the Lasso cross validated RMSE is better than XGBoost's CV score, I decided to weight the Lasso results double.

 이 커널은 세부적인 EDA와 다양한 시각화를 포함하며, 앙상블한 모델링이 포함된다.
 모델링은 Lasso와 XGBoost를 사용했으며, 교차 검증 결과 XGBoost - RMSE 0.1162, Lasso - 0.1121로 Lasso가 더 낫기에 Lasso에 weight를 2배를 주고 쓰기로 했다.
 
 <br>
 
#3 Loading and Exploring Data
 
##3.1 Loading libraries required and reading the data into R
 
 Loading R packages.

```{r} 
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
```

Reading the data into R

```{r}
train <- read.csv('./input/House_price/train.csv', stringsAsFactors = F)
test <- read.csv('./input/House_price/test.csv', stringsAsFactors = F)
```

##3.2 Data size and Structure

 Train Data는 character 변수와 integer 변수로 구성되어 있다. 대개의 Character 변수는 실제적으론 factor 변수이지만, 그들 중 대다수는 cleaning / feature engineering 이 필요하므로 character 변수로 R에 불러온다. 
 구조 확인 결과 81개의 변수에 1460개의 관측치가 있으며, 종속 변수는 'SalePrice' 변수이다.
 dim, str 함수로 구조를 확인하고, 세부 사항을 설명하겠다.
<br>

 참조) read.csv()는 read_csv()와 달리 character 변수가 자동으로 factor로 변환되어 불러온다. stringAsFactor 함수를 쓰면 character 그대로 불러온다.
```{r}
dim(train)               # data 구조 확인
str(train[,c(1:10, 81)]) # 종속 변수와 처음부터 10번째까지의 변수 확인
```
 
```{r}
test_labels <- test$Id  # 후에 submission을 위해 test의 id는 벡터에 두고, 변수는 삭제한다.
test$Id <- NULL
train$Id <- NULL
```

```{r}
test$SalePrice <- NA     # rbind 사전 작업으로 변수 개수 맞추기 위해 SalePrice 변수 생성
all <- rbind(train, test)
dim(all)
```

#4 Exploring some of the most important variables

Id 변수를 제외하고 79개의 독립 변수와, 1개의 종속 변수가 있다.

##4.1 The response variables: SalePrice

 차트를 보면 오른쪽으로 치우친 분포다. 이건 소수의 사람이 고가의 집에 영향을 끼치는 것으로 보이며,
모델링 전에 이 분포는 손을 볼 예정이다.
<br>

 참조) rbind 과정중 all에는 test의 SalePrice를 전부 NA로 넣었다. 그래서 is.na로 결측치 제외한 데이터를 로드하고 x축의 scale을 적용해서 차트를 만들었다.

```{r}
ggplot(data = all[!is.na(all$SalePrice),], aes(x = SalePrice)) +
  geom_histogram(fill = 'blue', binwidth = 10000) +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) #0~80만까지 10만 단위로 x축 표현(구분자 ,)
```

```{r}
summary(all$SalePrice) # 요약값 확인
# 참조) 75%까지의 분포가 21만대 안에서 이뤄지며, 평균은 18만, 중위값은 16만이다. 
# 참조) 최대값과는 차이가 나는데 추후 보정을 하겠다.
```

##4.2 The most important numeric predictors
 character 변수는 사용하기 전에 전처리를 해야 한다. 대략적인 느낌을 위해, 
SalePrice와 상관 계수가 높은 numeric 변수들을 선별하겠다.

###4.2.1 Correlations with SalePrice

 SalePrice와 양의 상관 관계를 가진 10개의 numeric 변수를 선정한다.(상관 계수 0.5 이상)

```{r}
numericVars <- which(sapply(all, is.numeric)) # index 벡터 numeric 변수 저장
numericVarNames <- names(numericVars) #이름 명명하여 변수 생성
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use='pairwise.complete.obs') # 전 numeric 변수의 상관 계수

# SalePrice와의 상관 계수 내림차순으로 정렬
cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))
# 상관 계수가 큰 변수만을 선택
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, 
               tl.col = 'black',   # 변수명 색깔
               tl.pos = 'lt',      # 변수명 왼쪽 표시
               number.cex = .7)    # matrix안 상관계수 text 크기
```

 행렬에서 보이는 많은 변수들중 상관 관계가 가장 높은 2개 독립 변수 'OverallQual', 'GrLivArea'를 시각화하겠다.
('GrLivArea'는 지하실 제외한 지상의 면적이다.)
다중공선성을 clear하게 하는 것 또한 한가지 이슈이다. 예를 들어, 'GarageCars' 와 'GarageArea'의 상관 계수는 매우 높은 0.89이며 이 수치는 'SalePrice'와도 유사하게 높다. SalePrice와 0.5 이상의 상관 계수를 가진 변수들은 이렇다.
'TotalBsmtSF' - '1stFlrSF' - 'FullBath' - 'TotRmsAbvGrd' - 'YearBuilt' - 'YearRemodAdd'.


###4.2.2 Overall Quality

 numeric 변수중 SalePrice와 상관 계수가 가장 큰(0.79) 변수이며, 집의 전반적인 재료와 마감의 평가로 수치는 1(아주 낮은) ~ 10(아주 높은)까지이다.

```{r}
ggplot(data = all[!is.na(all$SalePrice),], aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(col = 'blue') + labs(x = 'Overall Quality') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) #0~80만까지 10만단위로 y축 표현(구분자 ,)
```

 SalePrice와 Overall Quality간은 양의 상관 관계를 보이며, 전반적으로 상향 곡선의 형태이다.
이상치는 특출난 값은 보이지 않는다. 후에 아웃라이어의 후보가 있다면, 4 등급의 비싼 집일 것이다.

###4.2.3 Above Grade (Ground) Living Area (square feet)

 numeric 변수중 SalePrice와 상관 계수가 두 번째로 큰 변수이며, 많은 관측치가 있지만 큰 집은 일반적으로 비싸다.

```{r}
ggplot(data = all[!is.na(all$SalePrice),], aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 1000000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, #price 4500이상 텍스트 표기
  rownames(all), '')))
```

 면적 대비 가격이 낮은 두 개의 이상치 같은 것이 보인다.(524, 1299번). 
위험할 수 있기에 이것을 아웃라이어로 배제하는건 아직 안하겠다.
예를 들어 Overall Quality의 낮은 등급은 낮은 가격을 설명하지만, 보이는 바와 같이 2채의 집은 실제적으로 최고 등급의 집이다. 따라서 1299, 524 두 채의 집은 일단 이상치 후보로 두기로 한다.

```{r}
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
```
