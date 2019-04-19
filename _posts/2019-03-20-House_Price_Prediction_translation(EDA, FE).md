---
title: "Kaggle House Prices Prediction Competition with R (한글 번역) EDA & FE [1]"
author: "Creed Maestro"
date: "2019/03/20"
categories: Kaggle
tags: Kaggle, R, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github

---



# 5 Missing data, label encoding, and factorizing variables
[캐글 Rmd 링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)

## 5.1 Completeness of the data

 우선 결측치를 포함한 변수들부터 확인해 보겠다.

```{r}
NAcol <- which(colSums(is.na(all)) > 0)  # 모든 결측치 변수 생성
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE) #결측치 변수 별로 내림차순 정렬
cat('There are', length(NAcol), 'columns with missing values')
```

 'SalePrice'의 1459개의 결측치는 test 셋의 수와 완벽하게 일치한다. 이건 34개의 독립 변수에서 NA를 수정해야 한다는 것을 의미한다.
<br>

## 5.2 Imputing missing data {.tabset}

 결측치가 포함된 34개 독립 변수를 수정할려고 한다. 개수의 내림차순으로 작업을 할 것이다. 다른 변수와 유의미한 관계를 갖는 변수를 찾는다면, 그룹핑 할 것이다. 예를 들어 Pool, Garage, Basement 같은 변수들은 여러 변수가 있다.
변수가 많아 탭으로 구분 지어 놓으니 그저 넘기지말고 필요한 것들 만이라도 읽어주길 바란다.
Garage와 Basement는 유무의 차이에 의미가 있어 이 두 섹션은 확인해주길 바란다.
또한, 결측치는 character 타입은 integer 타입으로 변환했고, 순서형 변수나 순서가 없는 범주형은 factor로 변환했다.
'one-hot-encoding'(model.matrix 함수)을 써서 factor를 numeric으로 변환할 것이다.


### 5.2.1 Pool variables

**Pool Quality and the PoolArea variable**

 PoolQC는 가장 많은 결측치가 있다. 세부 사항은 아래와 같다.

PoolQC: Pool quality

    Ex  Excellent
    Gd  Good
    TA  Average/Typical
    Fa  Fair
    NA  No Pool

 결측치에 'No pool' 명시를 해야 하고, 또한 NA의 높은 수는 이것이 일반적이고,
수영장이 있는 집들은 소수라는 걸 보여준다.

```{r}
all$PoolQC[is.na(all$PoolQC)] <- 'None'
```

 이 변수들을 순서형으로 변환하고, 동일한 품질 수준을 쓰는 다양한 변수가 있으므로 이후에 사용하기 위해 벡터를 만들겠다.

```{r}
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
```

 revalue 함수를 사용해서 정수형으로 변환을 하겠다.

```{r}
all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)
```

 결측치는 없지만, Pool과 연관된 변수가 하나 더 있다. 'PoolArea' 변수이다. 여기엔 PoolQc가 없는 3채가 있다.
두 변수간의 명확한 관계가 있는지 확인을 위해 살펴봤지만 없었고, PoolQC가 없는 3변수는 
Overall Quality에 맞춰 변수를 부여했다.

```{r}
all[all$PoolArea > 0 & all$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]

all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2
```



### 5.2.2 Miscellaneous Feature

**Miscellanuous feature not coverd in other categories**

 Miscellanuous Feature 변수는 2814개의 결측치가 있고, 데이터 타입은 순서형이 아니기에 factor로 변환했다.

    Elev  Elevator
    Gar2  2nd Garage (if not described in garage section)
    Othr  Other
    Shed  Shed (over 100 SF)  #헛간
    TenC  Tennis Court
    NA    None
    
```{r}
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'  #MiscFeature 결측치 'None' 치환
all$MiscFeature <- as.factor(all$MiscFeature)

ggplot(all[!is.na(all$SalePrice),], aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) #막대 그래프 count 라벨링

table(all$MiscFeature)
```

 빈도를 볼 때, 그 변수는 무의미해 보인다. 헛간이 있는 낮은 가격은 'no Garage'일 것이라 추측한다.
테니스 코트가 있는 집은 비싸고, 단 한 곳 뿐이었다.



### 5.2.3 Alley

** type of alley access to property **

 2721개의 결측치가 보이며, 데이터 타입은 순서형이 아니기에 factor로 변환했다.

    Grvl  Gravel
    Pave  Paved
    NA    No alley access
  
```{r}
all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)
ggplot(all[!is.na(all$SalePrice),], aes(x = Alley, y = SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = comma)

table(all$Alley)
```



### 5.2.4 Fence 

** Fence quality **

 2348개의 결측치가 보이고, 순서형 변수로 보이기에 그대로 작업한다.

    GdPrv   Good Privacy
    MnPrv   Minimum Privacy
    GdWo    Good Wood
    MnWw    Minimum Wood/Wire
    NA      No Fence
  
```{r}
all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)
all[!is.na(all$SalePrice),] %>% 
  group_by(Fence) %>%  #Fence 그룹핑
  summarise(median = median(SalePrice), counts = n()) #Fence변수의 price 중위값, 개수 확인
```

 확인 결과 Fence가 순서형 변수가 아니어서(no fence is best), factor 타입으로 변환한다.

```{r}
all$Fence <- as.factor(all$Fence)
```



### 5.2.5 Fireplace variables

** Fireplace quality, and Number of fireplaces **

 Fireplace Qulity에는 1420개의 결측치가 보이지만, Fireplaces는 완벽하다.

** Fireplace quality **

 FireplaceQu 결측치의 수는 fireplaces 변수가 0인 수와 일치한다. 
이건 이 결측치를 'no fireplace'로 바꾸는데 무리가 없다는 걸 말한다. 이건 순서형 범주이고, 
Pool Quality를 위해 이전에 만들어둔 Qulaites 벡터를 쓰겠다.

     Ex	  Excellent - Exceptional Masonry Fireplace
     Gd	  Good - Masonry Fireplace in main level
     TA	  Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
     Fa 	Fair - Prefabricated Fireplace in basement
     Po 	Poor - Ben Franklin Stove
     NA 	No Fireplace 
   
```{r}
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)
```

** Number of fireplaces **

 Fireplaces는 정수형 변수이고, 결측치는 없기에 별도의 작업이 필요없다.

```{r}
table(all$Fireplaces)      #Fireplaces의 범주별 개수 확인
sum(table(all$Fireplaces)) #합산한 개수가 변수의 총 개수와 같은지 확인
```

**탭으로 구분지었기에 위로 돌아가서 다른 탭들을 확인바란다.**

### 5.2.6 Lot variables

 3개의 변수가 있으며 이 중 하나는 결측치가 있고, 2개는 결측치가 없다.

** LotFrontage: Linear feet of street connected to property **

 결측치는 486개이고, 각각 동네들의 중위값을 구해 이를 대체하겠다.

```{r}
ggplot(all[!is.na(all$LotFrontage),], 
       aes(x = as.factor(Neighborhood), y = LotFrontage)) +
       geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Text 45도 기울기, 높이는 1로 설정
```

 참조) 상기 확인된 결측치를 반복문을 돌려 모두 대체하겠다.

```{r}
for (i in 1:nrow(all)) {
        if(is.na(all$LotFrontage[i])){
          all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]],
                                                  na.rm = TRUE))
        }
}
```  

** LotShape: General shape of property **

 결측치가 없고, 값은 순서형 타입으로 보인다.

     Reg	Regular	
     IR1	Slightly irregular
     IR2	Moderately Irregular
     IR3	Irregular
   
```{r}
all$LotShape <- as.integer(revalue(all$LotShape, c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)))
table(all$LotShape)
sum(table(all$LotShape))
```

** LotConfig: Lot configuration **

 결측치가 없다. 순서형 타입으로 보였지만, 확인해보니 아니었다. 그래서 factor 타입으로 변환하겠다.

     Inside   Inside lot
     Corner   Corner lot
     CulDSac  Cul-de-sac
     FR2  Frontage on 2 sides of property
     FR3  Frontage on 3 sides of property
  
```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(LotConfig), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))
```

```{r}
all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)
sum(table(all$LotConfig))
```


### 5.2.7 Garage variables

** Garage 관련 7개 변수가 있다 **

 결측치의 개수는 2개 변수(GarageCars, GarageArea)가 1개, 1개 변수(GarageType)가 157개,
나머지 4개 변수가 159개이다.
먼저 159개의 결측치가 있는 ** GarageYrBlt:창고 건축년도 ** 의 결측치 값을 YearBuilt의 값으로 대체하겠다.
(리모델링이나 증축을 하지 않은 YearBuilt의 default값이 YearReomdAdd 변수와 유사하다.)

```{r}
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
```

 결측치는 문자형 타입이며 'no garage'를 의미하기에, 157개와 159개의 결측치를 가진 세 변수의 차이점을 찾아보겠다.

```{r}
#157개의 결측치가 159개 결측치의 변수와 동일한 관측치인지 확인해 보겠다.
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
```

```{r}
#나머지 2개의 관측치를 찾아보겠다.
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), 
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
```

 'GarageType'의 157개 결측치는 'GarageCondition', 'GarageQuality', 'GarageFinish'에 있는 결측치를 나타낸다.
2127과 2577 두채의 다른 집을 찾았다. 2127은 창고가 있지만, 2577은 없는 것처럼 보인다.
즉 총 158개의 집이 창고가 없으며, 2127의 결측치는 159개 세 변수의 가장 많은 값으로 대체하겠다.

```{r}
# 최빈도 값으로 결측치 대체
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

# 대체 후 값 확인
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
```

** GarageCars and GarageArea: 창고내 차 대수, 창고 면적 **

 상기 두 변수 다 1개의 결측치가 있으며, 그건 2577 house이다. 
이건 지금껏의 관측에서 찾아봤듯이 모든 Garage 변수에서 2577은 'no Garage'를 나타낸다.

```{r}
# 2577 house에 값을 할당
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

# 문자형 변수인 4개 변수의 결측치가 모두 158개인지 확인해보겠다.
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
```

 4개의 문자형 변수는 Garage와 연관이 있으며 모든 셋에 158개의 동일한 결측치가 있고, 이건 "No Garage"를 의미한다.
이 섹션에서 이걸 수정했다.

** GarageType: Garage 위치에 따른 타입 **

 순서형 변수가 아니기에 factor 타입으로 변환하겠다.

     2Types   More than one type of garage
     Attchd   Attached to home
     Basment  Basement Garage
     BuiltIn  Built-In (Garage part of house - typically has room above garage)
     CarPort  Car Port
     Detchd   Detached from home
     NA       No Garage
   
```{r}
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)
```

** GarageFinish: Garage 인테리어 완료 **

 순서형 변수이다.

     Fin  Finished
     RFn  Rough Finished  
     Unf  Unfinished
     NA   No Garage  
   
```{r}
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3) #문자형 수치형으로 변환

all$GarageFinish <- as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)
```

** GarageQual: Garage Quality **

 Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent
     Gd   Good
     TA   Typical/Average
     Fa   Fair
     Po   Poor
     NA   No Garage
   
```{r}
all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual <- as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)
```

** GarageCond: Garage condition **

 Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent
     Gd   Good
     TA   Typical/Average
     Fa   Fair
     Po   Poor
     NA   No Garage
   
```{r}
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond <- as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)
```
