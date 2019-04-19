---
title: "Kaggle House Prices Prediction Competition with R (한글 번역) EDA & FE [5]"
author: "Creed Maestro"
date: "2019/03/24"
categories: Kaggle
tags: Kaggle, R, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github

---


[캐글 Rmd 링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)


# 6 Visualization of important variables

 끝이 보인다. 모든 문자형 변수를 범주형 factor나, 라벨링하여 숫자형으로 인코딩했다.
더해서, 3개의 수치형 변수는 factor로 변환했고, 1개 변수(Utilities)는 삭제했다.
아래와 같이, 수치형 변수의 수는 이제 56개(종속 변수 포함), 남은 23개 변수는 범주형이다.

```{r}
numericVars <- which(sapply(all, is.numeric)) # index vector numeric variables
factorVars <- which(sapply(all, is.factor))   # index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 
    'categoric variables')
```

## 6.1 Correlations again

 상관 계수를 다시 확인해 보니, 상관 계수 0.5 이상의 변수가 10개에서 16개로 늘었다.

```{r}
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = 'pairwise.complete.obs') # 모든 수치형 변수의 상관 계수

# SalePrice와 변수들의 상관 계수 내림차순 정렬
cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))
# 0.5보다 높은 상관 계수 취사 선택
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, 
               tl.col = 'black',  # 변수명 색깔
               tl.pos = 'lt',     # 변수명 왼쪽 표시
               tl.cex = 0.7,      # 변수명 text 크기
               cl.cex = 0.7,      # y축 상관계수 text 크기
               number.cex = .7    # matrix안 상관계수 text 크기
               )
```

## 6.2 Finding variable importance with a quick Random Forest

 상기의 상관 행렬은 가장 중요한 수치형 변수와 그 변수들 사이의 다중공선성을 잘 보여주지만,
시각화를 하기 전에 범주형 변수까지 포함하여 가장 중요한 변수에 대한 개요를 얻고 싶었다.

 패키지의 calc.relimp function, boruta function으로 빠르게 선형 회귀 분석을 하여 중요 변수와 그렇지 않은 변수를 
그룹핑으로 나누어 연관성있는 중요 변수를 얻을려고 했다.그러나, 이건 너무 많은 시간이 걸렸다. 
그저 변수의 중요성에 대한 시사만 얻으면 되기에,단지 100개의 tree만을 써서 깔끔하진 못하지만 빠른 Random Forest 모델을 간략하게 사용하기로 했다.
이 작업은 상대적으로 적은 수의 tree만을 쓰기에 시간이 그리 오래 걸리지 않았다.

```{r}
set.seed(2018) #인수 '2018'로 시드 생성
quick_RF <- randomForest(x = all[1:1460, -79], y = all$SalePrice[1:1460], ntree = 100, importance = TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,],
       aes(x = reorder(Variables, MSE), y = MSE, fill = MSE)) + # MSE기준 변수 재정렬
       geom_bar(stat = 'identity') + 
       labs(x = 'Variables', y = '% increase MSE if variable is randomly permuted') + #x,y축명 명명
       coord_flip() + #x, y축 반전
       theme(legend.position = 'none')
```

 가장 중요한 변수중 3개만이 범주형이다. 'Neighborhood', 'MSSubClass', 'GarageType'

### 6.2.1 Above Ground Living Area, and other surface related variables (in square feet)

 처음 데이터를 볼 때 Above Ground Living Area와 SalePrice 변수간의 연관성은 이미 확인했지만,
그 분포를 다시 나타내겠다. 이번 섹션에서 'Square feet' 연관 변수중 측정된 상위 20개의 값으로 번들을 만들겠다.
Note: GarageArea 변수는 Garage variables 섹션에서 다루겠다.

 'Total Rooms Above Ground(TotRmsAbvGrd)' 변수 또한 Above Ground Living Area와 높은 상관 관계(0.81)를
보이기에 더해서 나타낸다.

```{r}
s1 <- ggplot(data = all, aes(x = GrLivArea)) +
  geom_density() + labs(x = 'Square feet living area')
s2 <- ggplot(data = all, aes(x = as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat = 'count') + labs(x = 'Rooms above Ground')
s3 <- ggplot(all, aes(x = X1stFlrSF)) +
  geom_density() + labs(x = 'Square feet first floor')
s4 <- ggplot(all, aes(x = X2ndFlrSF)) + 
  geom_density() + labs(x = 'Square feet second floor')
s5 <- ggplot(all, aes(x = TotalBsmtSF)) +
  geom_density()+ labs(x = 'Square feet basement')
s6 <- ggplot(all[all$LotArea < 100000,], aes(x = LotArea)) + 
  geom_density() + labs(x = 'Square feet lot')
s7 <- ggplot(all, aes(x = LotFrontage)) +
  geom_density() + labs(x = 'Linear feet lot frontage')
s8 <- ggplot(all, aes(x = LowQualFinSF)) +
  geom_histogram() + labs(x = 'Low quality square feet 1st & 2nd')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2, byrow = TRUE) # 4행 2열 ()안의 순으로 행부터 matrix 생성
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout = layout)
```

 후에 이상치에 대한 몇 몇 변수를 조사하겠다. lot 시각화를 위해 이미 10만 평방 미터 이상의 4채를 제거했다.
GrLivArea는 1,2층 평방 피트의 합계인 것 같다. 그러나, 뒤에 있는 차트에서 LowQualFinSF라는 다른 이름의 변수를 찾았다.
이건 모든 층이 저품질로 완료된 평방 피트이다. 'Low quality square feet 1st and 2nd'를 보는 바와 같이 
40채를 제외하고 거의 모든 집이 해당되지 않는다. 이 평방 피트는 실제적으로 GrLivArea에 포함되어 있다.
GrLivArea와 3개 변수간의 상관계수는 정확하게 1이다.

```{r}
cor(all$GrLivArea, (all$X1stFlrSF + all$X2ndFlrSF + all$LowQualFinSF))

head(all[all$LowQualFinSF > 0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])
```

### 6.2.2 The most important categorical variables; Neighborhood

 처음의 그래프는 Neighborhood의 SalePrice 중위값을 보여준다. 
<br>
참조) 동네에 따른 가격으로 설명할 수 있을 것 같다.

train data 기준 각각의 동네에 따른 집의 개수를 라벨링하였다.(test data엔 SalePrice가 결측치.)

두 번째 그래프는 all data 기준 Neighborhood에 따른 집의 개수를 라벨링하였다.

```{r}
n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x = Neighborhood, y = SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') #점선은 판매가 중위값

n2 <- ggplot(all, aes(x = Neighborhood)) +
  geom_histogram(stat = 'count') + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(n1, n2)
```

### 6.2.3 Overall Quality, and other Quality variables

 처음 데이터를 볼 때 Overall Quality와 SalePrice 변수간의 관계성은 이미 시각화했지만, 
빈도 분포도 나타내고 싶다. 이번 섹션에선 다른 Quality들도 번들로 묶어 나타내겠다.

```{r}
q1 <- ggplot(all, aes(x = as.factor(OverallQual))) + 
  geom_histogram(stat = 'count')
q2 <- ggplot(all, aes(x = as.factor(ExterQual))) + 
  geom_histogram(stat = 'count')
q3 <- ggplot(all, aes(x = as.factor(BsmtQual))) + 
  geom_histogram(stat = 'count')
q4 <- ggplot(all, aes(x = as.factor(KitchenQual))) + 
  geom_histogram(stat = 'count')
q5 <- ggplot(all, aes(x = as.factor(GarageQual))) +
  geom_histogram(stat = 'count')
q6 <- ggplot(all, aes(x = as.factor(FireplaceQu))) + 
  geom_histogram(stat = 'count')
q7 <- ggplot(all, aes(x = as.factor(PoolQC))) +
  geom_histogram(stat = 'count')

layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3, byrow = TRUE) #3행 3열 matrix
multiplot(q1, q2, q3, q4, q5, q6, q7, layout = layout)
```

 Overall Quality는 아주 중요하고 다른 변수들보다 범주가 많다. 
External Quality 또한 중요하지만 Overall Quality와 상관 계수가 높다.(0.73)
Kitchen Quality는 범주마다 약간의 차이는 있지만 모든 집이 주방이 있다는 걸 보여준다.
Garage Quality는 대부분이 Q3에 몰려 있어 구분이 쉽지 않다.
Fireplace Quality는 높은 상관 계수 항목과 중요한 변수 항목안에 있다.
PoolQC는 매우 드물다.(이 규모에선 13개 풀은 보이지조차 않는다.) 나중에 pool 유무를 변수로 만들겠다.

### 6.2.4 The second most important categoriacal variables; MSSubClass

 첫 번째 차트는 MSSubClass의 SalePrice 중위값을 시각화한 것이다. 
train data 기준 각각의 MsSubClass에 따른 집의 개수를 라벨링하였다.
히스토그램은 all data 기준 MSSubClass에 따른 집의 개수를 라벨링하였다.
대다수의 집은 상대적으로 신축이며, 1개 또는 2개의 이야기를 가진다.

```{r}
ms1 <- ggplot(all[!is.na(all$SalePrice),], aes(x = MSSubClass, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') #점선은 판매가 중위값

ms2 <- ggplot(all, aes(x = MSSubClass)) +
  geom_histogram(stat = 'count') + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)
```

### 6.2.5 Garage variables

 몇 몇 Garage 변수는 SalePrice와 높은 상관 관계를 가지고 있고, quick random forest의 top20 리스트에도 들어 있다.
그러나 그들간에 다중 공선성이 있고, 7개 Garage 변수는 너무 많아 보인다.
'GarageCars', 'GarageType'과 'Quality 측정 변수' 중 하나, 이렇게 3개면 충분하다고 생각한다.
그럼에도 3개를 선별하기 전에 일단 7개를 모두 시각화하겠다.

```{r}
# 오탈자 수정
all$GarageYrBlt[2593] <- 2007 # GarageYrBlt=2207, YearBuilt = 2006, YearRemodAdd= 2007.
# data가 잘못 입력된 data이다. 주최측에서 실수가 있었던것 같다. 재건축 년도 기준으로 맞추겠다.
```

```{r}
g1 <- ggplot(all[all$GarageCars !=0,], aes(x = GarageYrBlt)) +
  geom_histogram()
g2 <- ggplot(all, aes(x = as.factor(GarageCars))) +
  geom_histogram(stat = 'count')
g3 <- ggplot(all, aes(x = GarageArea)) +
  geom_density()
g4 <- ggplot(all, aes(x = as.factor(GarageCond))) + 
  geom_histogram(stat = 'count')
g5 <- ggplot(all, aes(x = GarageType)) +
  geom_histogram(stat = 'count')
g6 <- ggplot(all, aes(x = as.factor(GarageQual))) +
  geom_histogram(stat = 'count')
g7 <- ggplot(all, aes(x = as.factor(GarageFinish))) +
  geom_histogram(stat = 'count')

layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3, byrow = TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout = layout)
```

 4.2 섹션에서 이미 설명을 했듯이 GarageCars와 GarageArea는 높은 상관 관계를 가진다.
여기에선 GarageQual과 GarageCond 역시 높은 상관 관계를 가지는 걸로 보이고, 3레벨의 개수가 가장 많다.

### 6.2.6 Basement variables

 Garage 변수와 비슷하게, 상관 행렬과 top20 RF 독립 변수 list 내의 다양한 basement 변수들도 중요하다.
그러나, 11 Basement 변수는 너무 과잉으로 보인다. 작업하기 전에 이 중 8개를 시각화 하겠다
'2개의 Bathroom 변수'는 7.1 섹션에서 다루고, 'Basement square feet' 변수는 6.2.1 섹션에서 다루었다.

```{r}
b1 <- ggplot(all, aes(x = BsmtFinSF1)) +
  geom_histogram() + labs(x = 'Type 1 finished square feet')
b2 <- ggplot(all, aes(x = BsmtFinSF2)) +
  geom_histogram() + labs(x = 'Type 2 finished square feet')
b3 <- ggplot(all, aes(x = BsmtUnfSF)) +
  geom_histogram() + labs(x = 'Unfinished square feet')
b4 <- ggplot(all, aes(x = as.factor(BsmtFinType1))) +
  geom_histogram(stat = 'count') + labs(x = 'Rating of Type 1 finished area')
b5 <- ggplot(all, aes(x = as.factor(BsmtFinType2))) + 
  geom_histogram(stat = 'count') + labs(x = 'Rating of Type 2 finished area')
b6 <- ggplot(all, aes(x = as.factor(BsmtQual))) +
  geom_histogram(stat = 'count') + labs(x = 'Height of the basement')
b7 <- ggplot(all, aes(x = as.factor(BsmtCond))) + 
  geom_histogram(stat = 'count') + labs(x = 'Rating of general condition')
b8 <- ggplot(all, aes(x = as.factor(BsmtExposure))) +
  geom_histogram(stat = 'count') + labs(x = 'Walkout or garden level walls')

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3, byrow= TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout = layout)
```

 보는 바와 같이 'TotalBsmtSF' 변수의 총 지하 면적이 완료 영역(2가지 이상의 유형이 있는 경우 2개) & 
미완 영역으로 세분화 된 것 처럼 보인다. 3 변수(BsmtFinSF1, SF2, UnfSf)와 TotalBsmtSf 간의 상관 계수를 확인해보자
그 상관 계수는 정확하게 1이다. 에러도 작은 차이도 없는 멋진 수치다!

Basement Quality는 지하실 높이를 구체적으로 평가한다는 점에서 혼란을 주는 변수 명이다.

# 7 Feature engineering

## 7.1 Total number of Bathrooms

 4개의 Bathroom 관련 변수는 개별적으론 그다지 중요하지 않아 보인다. 
하지만, 만약 여기에 하나의 독립 변수를 추가한다면 그건 아주 강력한 요인이 될 것이다

파우더룸이나 게스트용 Bath로 알려진 'half-bath', 여기엔 욕실의 주요 구성 요소 4개 중 2개만이 있다.
(화장실과 싱크대) 결론적으로, half bathroom을 0.5로 정해서 계산할 것이다.

```{r}
all$ToBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)
```

 첫 번째 그래프에서 보이듯이, 추가한 변수는 깔끔한 0.63의 상관 계수를 보인다.
두 번째 그래프는 all data기준, Bathroom의 개수 분포를 나타낸다.

```{r}
tb1 <- ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(ToBathrooms), y = SalePrice)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) + 
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
tb2 <- ggplot(all, aes(x = as.factor(ToBathrooms))) + 
  geom_histogram(stat = 'count')
grid.arrange(tb1, tb2)
```

## 7.2 Adding 'House Age', 'Remodeled (Yes/No)', and IsNew Variables

 집의 년수와 관련된 3개의 변수가 있다. 'YearBlt', 'YearRemodAdd', 'YearSold'
증축/리모델링을 하지 않을 시 'YearRemodAdd'는 'YearBuilt'의 값으로 기본 설정된다.
년수를 결정하기 위해 'YearRemodeled'와 'YearSold'를 쓸 것이다.
그러나, 오래된 구조물의 일부가 남아있고, 집의 일부만 개조되었을 수 있으므로 리모델링 유무 변수에 흥미가 간다.
만약 건축 년수가 리모델링 날짜에 기반한다면 이건 패널티 변수로 보아야 한다.
그건 아마도 같은 해에 신축된 건물보다 가치가 낮을 것이기 때문이다.

```{r}
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) # 0 = 'No remodeling', 1 = 'remodeling'
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
```

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = Age, y = SalePrice)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
```

 예상대로 그래프는 년수와 판매 가격간 음의 상관 관계를 보여준다.(오래된 집은 가치가 낮다.)

```{r}
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])
```

 아래와 같이 예상대로 리모델링 건물은 그렇지 않은 건물보다 가치가 떨어진다.

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(Remod), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 6) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) + 
  theme_grey(base_size = 18) + 
  geom_hline(yintercept = 163000, linetype = 'dashed') # 점선은 판매가의 중위값
```

 마지막으로, 아래와 같이 IsNew 변수를 만들겠다. 데이터셋 안에 116채의 집이 새 집이다.

```{r}
all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0) #판매 년도와 건축 년도가 같으면 1, 아니면 0
table(all$IsNew)
```

 이 116채의 새 집은 train과 test 셋에 공평하게 분배되고, 보는 바와 같이 새 집들은 평균보다 훨씬 가치가 있다.

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(IsNew), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size =6) + 
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept = 163000, linetype = 'dashed') # 점선은 판매가의 중위값
```

```{r}
all$YrSold <- as.factor(all$YrSold) # numeric 변수 버전은 더이상 필요하지 않다.
```

## 7.3 Binning Neighborhood

```{r}
nb1 <- ggplot(all[!is.na(all$SalePrice),], 
              aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
  labs(x = 'Neighborhood', y = 'Median SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) + 
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') #점선은 판매가의 중위값
nb2 <- ggplot(all[!is.na(all$SalePrice),], 
              aes(x = reorder(Neighborhood, SalePrice, FUN = mean), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = 'blue') + 
  labs(x = 'Neighborhood', y = 'Mean SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma ) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..), size = 3) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') #점선은 판매가의 중위값
grid.arrange(nb1, nb2)
```

 대체적으로 판매가가 높은 3 동네는 중위값과 평균의 순위 역시 높게 보인다. 
상대적으로 제일 열악한 3곳의 동네에서의 양쪽 그래프 역시 같게 보인다.
'overbin'하고 싶지 않기 때문에, 극단에 해당하는 범주만 만들겠다.

```{r}
all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
```

```{r}
table(all$NeighRich)
```

## 7.4 Total Square Feet

 사람들이 집을 살 때 일반적으로 총 거주 공간은 아주 중요하다. 지상 + 지하의 공간을 더해 독립 변수를 만들겠다.

```{r}
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF
```

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = TotalSqFeet, y = SalePrice)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all), '')))
```

 예상한 바와 같이 판매가와의 상관 관계는 아주 강력하다(0.78)

```{r}
cor(all$SalePrice, all$TotalSqFeet, use = 'pairwise.complete.obs') #결측치있는 case 제거한 상관계수 계산
```

 두 개의 잠재적인 이상치는 이전보다 더 극단적으로 보인다. 두 이상치를 제거하면 상관 계수가 5% 증가할 것이다.

```{r}
cor(all$SalePrice[-c(524, 1299)], all$TotalSqFeet[-c(524, 1299)], use = 'pairwise.complete.obs')
```

## 7.5 Consolidating Porch variables

 아래와 같이, 현관(Porch)과 관련된 변수들을 리스트했다.

* WoodDeckSF: Wood deck area in square feet

* OpenPorchSF: Open porch area in square feet

* EnclosedPorch: Enclosed porch area in square feet

* 3SsnPorch: Three season porch area in square feet

* ScreenPorch: Screen porch area in square feet

 내가 아는 한, 현관은 집 외부의 보호구역이고, Wooden deck은 보호가 되지 않는다.
따라서, Wooden deck은 제외하고 4개 변수만을 고려하겠다.

```{r}
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
```

 이런 현관 영역을 합산하는 것은 의미가 있지만(영역간에 겹쳐서는 안된다) 판매가와의 그 상관 관계는 높지 않다.

```{r}
cor(all$SalePrice, all$TotalPorchSF, use = 'pairwise.complete.obs')
```

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = TotalPorchSF, y = SalePrice)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se=FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
```
