---
title: "House Prices Prediction with R (한글 번역) EDA & FE "
author: "Maestro"
date: "2019년 3월 20일"
categories: Kaggle
tags: Kaggle
layout: post

---

#5 Missing data, label encoding, and factorizing variables

##5.1 Completeness of the data

 우선 결측치를 포함한 변수들부터 확인해 보겠다.

```{r}
NAcol <- which(colSums(is.na(all)) > 0)  # 모든 결측치 변수 생성
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE) #결측치 변수 별로 내림차순 정렬
cat('There are', length(NAcol), 'columns with missing values')
```

 'SalePrice'의 1459개의 결측치는 test 셋의 수와 완벽하게 일치한다. 이건 34개의 독립 변수에서 NA를 수정해야 한다는 것을 의미한다.
<br>

##5.2 Imputing missing data {.tabset}

 결측치가 포함된 34개 독립 변수를 수정할려고 한다. 개수의 내림차순으로 작업을 할 것이다. 다른 변수와 유의미한 관계를 갖는 변수를 찾는다면, 그룹핑 할 것이다. 예를 들어 Pool, Garage, Basement 같은 변수들은 여러 변수가 있다.
변수가 많아 탭으로 구분 지어 놓으니 그저 넘기지말고 필요한 것들 만이라도 읽어주길 바란다.
Garage와 Basement는 유무의 차이에 의미가 있어 이 두 섹션은 확인해주길 바란다.
또한, 결측치는 character 타입은 integer 타입으로 변환했고, 순서형 변수나 순서가 없는 범주형은 factor로 변환했다.
'one-hot-encoding'(model.matrix 함수)을 써서 factor를 numeric으로 변환할 것이다.


###5.2.1 Pool variables

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



###5.2.2 Miscellaneous Feature

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



###5.2.3 Alley

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



###5.2.4 Fence 

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



###5.2.5 Fireplace variables

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

###5.2.6 Lot variables

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



###5.2.7 Garage variables

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


###5.2.8 Basement variables

** Basement 관련 변수는 11개이다 **

 이 중 5개 변수는 79 - 82개의 결측치고, 6개 변수는 1개 또는  2개의 결측치가 있다.

```{r}
# 79개의 결측치가 80 이상의 결측치의 값과 동일한 관측치를 보이는지 확인하겠다.
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & 
               is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

# 추가 결측치 찾기: BsmtFinType1은 결측치가 아니지만, 다른 4개 변수들이 결측치인 경우
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond) | is.na(all$BsmtQual) | 
                                is.na(all$BsmtExposure) | is.na(all$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
```

 확인 결과, 79채가 지하실이 없는 것처럼 보인다. 그 중 9채는 중간 중간 결측치가 보이는데,
이건 각 변수를 내림차순으로 정렬해 가장 빈도수가 높은 값으로 대체하겠다.

```{r}
# 최빈도값으로 대체값 할당
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtQual[c(2218,2219)] <- names(sort(-table(all$BsmtQual)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
```

 79~82개의 결측치를 가진 5개 변수는 지하실이 없어 보여, factorize/ hot encode하겠다.

** BsmtQual: 지하실 높이를 등급화 **

 Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent (100+ inches) 
     Gd   Good (90-99 inches)
     TA   Typical (80-89 inches)
     Fa   Fair (70-79 inches)
     Po   Poor (<70 inches
     NA   No Basement
   
```{r}
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual <- as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)
```

** BsmtCond: 지하실의 일반적인 상태로 본 등급 **

 Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent
     Gd   Good
     TA   Typical - slight dampness allowed
     Fa   Fair - dampness or some cracking or settling
     Po   Poor - Severe cracking, settling, or wetness
     NA   No Basement
   
```{r}
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond <- as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)
```

** BsmtExposure: 벽이나 정원의 도보 적합도 수준 **

 변수는 순서형 타입이다.

     Gd   Good Exposure
     Av   Average Exposure (split levels or foyers typically score average or above)  
     Mn   Mimimum Exposure
     No   No Exposure
     NA   No Basement
   
```{r}
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)
```

** BsmtFinType1: 지하실 완공 면적의 등급 **

 변수는 순서형 타입이다.

     GLQ  Good Living Quarters
     ALQ  Average Living Quarters
     BLQ  Below Average Living Quarters   
     Rec  Average Rec Room
     LwQ  Low Quality
     Unf  Unfinshed
     NA   No Basement
   
```{r}
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
Fintype <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, Fintype))
table(all$BsmtFinType1)
```

** BsmtFinType2: multiple type일 경우의 지하실 완공 면적 등급 **

 변수는 순서형 타입이고, Fintype 벡터이다.

     GLQ  Good Living Quarters
     ALQ  Average Living Quarters
     BLQ  Below Average Living Quarters   
     Rec  Average Rec Room
     LwQ  Low Quality
     Unf  Unfinshed
     NA   No Basement
   
```{r}
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, Fintype))
table(all$BsmtFinType2)
```

** 결측치 1~2개의 남은 6개 변수 **

 소수의 결측치를 가진 남은 6개 변수를 확인해보자

```{r}
# 상기에 관측했던 지하실이 없었던 79채를 참고하여 남은 결측치를 확인해보자
all[(is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) | is.na(all$BsmtFinSF1) | 
       is.na(all$BsmtFinSF2) | is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
```

 확인 결과 2채의 집에 결측치를 발견했고, 결측치는 0으로 대체하겠다.

** BsmtFullBath: full bathrooms을 갖춘 지하실 **

 정수형 타입의 변수이다.

```{r}
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)
```

** BsmtHalfBath: half bathrooms을 갖춘 지하실 **

 정수형 타입의 변수이다.

```{r}
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
table(all$BsmtHalfBath)
```

** BsmtFinSF1: 크기가 1 평방 미터인 지하실 **

 정수형 타입의 변수이다.

```{r}
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
```

** BsmtFinSF2: 크기가 2 평방 미터인 지하실 **

 정수형 타입의 변수이다.

```{r}
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
```

** BsmtUnfSF: 면적이 평방 미터로 측정이 안되는 지하실**

 정수형 타입의 변수이다.

```{r}
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
```

** TotalBsmtSF: 지하실의 총 평방 미터**

 정수형 타입의 변수이다.

```{r}
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0
```


###5.2.9 Masonry variables

** Masonry veneer type, and masonry veneer area **

 결측치가 Masonry veneer type은 24개, veneer area는 23개이다. veneer area를 가진 집이면,
veneer type일텐데 결측치 개수가 맞지 않는게 이상하다. 수정을 해보자

```{r}
# veneer area의 23개 결측치가 veneer type의 23개 결측치인지 확인해 보겠다. 
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
```

```{r}
# veneer type의 1개 결측치를 찾아보자.
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
```

```{r}
#veneer type의 결측치를 최빈도값으로 대체하자
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #최빈도는 'none'이라 두번째 빈도로 했다.
all[2611, c('MasVnrType', 'MasVnrArea')]
```

 veneer type과 veneer area의 동일한 23개 결측치는 석조가 없는걸 의미한다(no masonry)

** Masonry veneer type **

 범주는 아래와 같다.

     BrkCmn   Brick Common
     BrkFace  Brick Face
     CBlock   Cinder Block
     None     None
     Stone    Stone
   
```{r}
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice),] %>%    #결측치 아닌 SalePrice 한정
  group_by(MasVnrType) %>%         # MasVnrType 변수로 그룹핑
  summarise(median = median(SalePrice), counts=n()) %>%  # SalePrice 중위값, 개수 요약
  arrange(median)                  # 중위값 순으로 오름차순 정렬
```

 'common brick/none'과 다른 타입간에 중요한 차이가 보인다. 
심플한 돌이나 목조로 만든 주택이 저렴할 걸로 추정된다. 확인해 보겠다.

```{r}
Masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)
```

** MasVnrArea: 평방 미터 안의 Masonry veneer area **

 정수형 변수이다.

```{r}
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
```


###5.2.10 MS Zoning

** MSZoning: 용도별 지구 식별자 **

 결측치는 4개이고, 값은 범주형이다.

    A    Agriculture
    C    Commercial
    FV   Floating Village Residential
    I    Industrial
    RH   Residential High Density
    RL   Residential Low Density
    RP   Residential Low Density Park
    RM   Residential Medium Density
     
```{r}
# 최빈도값으로 결측치 대체
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)
sum(table(all$MSZoning))
```


###5.2.11 Kitchen variables

** Kitchen quality and number of Kitchens above grade **

 Kitchen quality 는 결측치가 1개, number of Kitchen 은 결측치가 없다.

** Kitchen quality **

 1개의 결측치가 있고, qualities vector로 순서형으로 변환하겠다.

     Ex   Excellent
     Gd   Good
     TA   Typical/Average
     Fa   Fair
     Po   Poor
   
```{r}
# 최빈도값으로 결측치 대체
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' 
all$KitchenQual <- as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)
sum(table(all$KitchenQual))
```

** Number of Kitchens above grade **

 순서형 변주이고, 결측치는 없다.

```{r}
table(all$KitchenAbvGr)
sum(table(all$KitchenAbvGr))
```

**탭으로 구분지었기에 위로 돌아가서 다른 탭들을 확인바란다.**

###5.2.12 Utilities

** Utilities: 사용할 수 있는 Utilities의 종류 **

 결측치는 2개이다.

     AllPub   All public Utilities (E,G,W,& S)    
     NoSewr   Electricity, Gas, and Water (Septic Tank)
     NoSeWa   Electricity and Gas Only
     ELO  Electricity only

 관측된 데이터중 한 집을 제외하곤 모두 AllPub 이다. 한 집은 train 데이터에만 있고, test에는 없다.
결측치를 'AllPub'으로 대체하는 것은 test set 의 모든 집이 'AllPub'임을 의미한다.
prediction때 이 변수를 사용하지 않기 위해 이 변수는 지우겠다.

```{r}
table(all$Utilities)
kable(all[is.na(all$Utilities) | all$Utilities == "NoSeWa", 1:9])
all$Utilities <- NULL
```


###5.2.13 Home functionality
 
** Functional: 홈 기능 **
 
 결측치는 1개이고, 순서형 타입으로 변환하겠다. (Sal이 가장 안좋고, Typ가 가장 좋다.)

     Typ  Typical Functionality
     Min1 Minor Deductions 1
     Min2 Minor Deductions 2
     Mod  Moderate Deductions
     Maj1 Major Deductions 1
     Maj2 Major Deductions 2
     Sev  Severely Damaged
     Sal  Salvage only
   
```{r}
# 최빈도값으로 결측치 대체
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional <- as.integer(revalue(all$Functional, +
                  c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)))
table(all$Functional)
sum(table(all$Functional))
```


###5.2.14 Exterior variables

** 건물 외장 변수로 4개가 있다. **

 2개의 변수는 결측치가 1개이고, 2개의 변수는 결측치가 없다.

** Exterior1st: 건물 외장 형태**

 결측치는 1개이며, 값은 범주형이다.

     AsbShng  Asbestos Shingles
     AsphShn  Asphalt Shingles
     BrkComm  Brick Common
     BrkFace  Brick Face
     CBlock   Cinder Block
     CemntBd  Cement Board
     HdBoard  Hard Board
     ImStucc  Imitation Stucco
     MetalSd  Metal Siding
     Other    Other
     Plywood  Plywood
     PreCast  PreCast 
     Stone    Stone
     Stucco   Stucco
     VinylSd  Vinyl Siding
     Wd Sdng  Wood Siding
     WdShing  Wood Shingles
   
```{r}
# 최빈도값으로 결측치 대체
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)
sum(table(all$Exterior1st))
```

** Exterior2nd: 건물 외장 형태(2개 이상의 형태를 쓴 경우) **

 결측치는 1개이며, 값은 범주형이다.

     AsbShng  Asbestos Shingles
     AsphShn  Asphalt Shingles
     BrkComm  Brick Common
     BrkFace  Brick Face
     CBlock   Cinder Block
     CemntBd  Cement Board
     HdBoard  Hard Board
     ImStucc  Imitation Stucco
     MetalSd  Metal Siding
     Other    Other
     Plywood  Plywood
     PreCast  PreCast 
     Stone    Stone
     Stucco   Stucco
     VinylSd  Vinyl Siding
     Wd Sdng  Wood Siding
     WdShing  Wood Shingles
   
```{r}
# 최빈도값으로 결측치 대체
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)
sum(table(all$Exterior2nd))
```

** ExterQual: 사용한 외장 형태의 등급 **

 결측치는 없고, Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent
     Gd   Good
     TA   Average/Typical
     Fa   Fair
     Po   Poor
   
```{r}
all$ExterQual <- as.integer(revalue(all$ExterQual, Qualities))
table(all$ExterQual)
sum(table(all$ExterQual))
```

** Extercond: 현재 외장의 상태 등급 **

 결측치는 없고, Qualities 벡터로 순서형으로 변환한다.

     Ex   Excellent
     Gd   Good
     TA   Average/Typical
     Fa   Fair
     Po   Poor
     
```{r}
all$ExterCond <- as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)
sum(table(all$ExterCond))
```


###5.2.15 Electrical system

** Electrical: 전기 시스템 **

 결측치는 1개이고, 값은 범주형이다.

     SBrkr    Standard Circuit Breakers & Romex
     FuseA    Fuse Box over 60 AMP and all Romex wiring (Average) 
     FuseF    60 AMP Fuse Box and mostly Romex wiring (Fair)
     FuseP    60 AMP Fuse Box and mostly knob & tube wiring (poor)
     Mix      Mixed
   
```{r}
# 최빈도값으로 결측치 대체
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)
sum(table(all$Electrical))
```


###5.2.16 SaleType and Condition

** SaleType: 판매 방식 **

 결측치는 1개이고,값은 범주형이다.

     WD       Warranty Deed - Conventional
     CWD      Warranty Deed - Cash
     VWD      Warranty Deed - VA Loan
     New      Home just constructed and sold
     COD      Court Officer Deed/Estate
     Con      Contract 15% Down payment regular terms
     ConLw    Contract Low Down payment and low interest
     ConLI    Contract Low Interest
     ConLD    Contract Low Down
     Oth      Other

```{r}
# 최빈도값으로 결측치 대체
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
sum(table(all$SaleType))
```

** SaleCondition: 판매 조건 **

 결측치는 없고, 값은 범주형이다.

     Normal   Normal Sale
     Abnorml  Abnormal Sale -  trade, foreclosure, short sale
     AdjLand  Adjoining Land Purchase
     Alloca   Allocation - two linked properties with separate deeds, typically condo with a garage unit  
     Family   Sale between family members
     Partial  Home was not completed when last assessed (associated with New Homes)
   
```{r}
all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))
```


##5.3 Label encoding/ factorizing the ramaining character variables {.tabset}

 결측치는 다 살펴보았지만, character 타입이었던 변수들은 아직 확인이 되지 않았다.
그래서 이번에도 탭으로 구분하여 살펴 보겠다.

```{r}
Charcol <- names(all[,sapply(all, is.character)]) #문자형 변수만 선별하여 생성
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')
```

###5.3.1 Foundation

** Foundation: 건물 기초(토대)의 종류 **

    BrkTil          Brick & Tile
    CBlock          Cinder Block
    PConc           Poured Contrete 
    Slab            Slab
    Stone           Stone
    Wood            Wood
    
```{r}
#순서형이 아니기에, factor형으로 변환하겠다.
all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)
sum(table(all$Foundation))
```


###5.3.2 Heating and airco

 2개의 Heating 변수와, 1개의 에어컨 변수(Y/N)가 있다.

** Heating: 난방 장치의 종류 **

     Floor    Floor Furnace
     GasA Gas forced warm air furnace
     GasW Gas hot water or steam heat
     Grav Gravity furnace 
     OthW Hot water or steam heat other than gas
     Wall Wall furnace
   
```{r}
#순서형이 아니기에, factor형으로 변환하겠다.
all$Heating <- as.factor(all$Heating)
table(all$Heating)
sum(table(all$Heating))
```

** HeatingQC: 난방 장치의 품질과 상태 **

     Ex   Excellent
     Gd   Good
     TA   Average/Typical
     Fa   Fair
     Po   Poor
   
```{r}
# Qualities 벡터로 순서형으로 변환한다.
all$HeatingQC <- as.integer(revalue(all$HeatingQC, Qualities))
table(all$HeatingQC)
sum(table(all$HeatingQC))
```

** CentralAir: 중앙 냉방 에어컨 유무 **

     N    No
     Y    Yes
   
```{r}
all$CentralAir <- as.integer(revalue(all$CentralAir, c('N' = 0, 'Y' = 1)))
table(all$CentralAir)
sum(table(all$CentralAir))
```


###5.3.3 Roof

 2개의 변수가 있다.

** RoofStyle: 지붕의 종류 **

     Flat Flat
     Gable    Gable
     Gambrel  Gabrel (Barn)
     Hip  Hip
     Mansard  Mansard
     Shed Shed
   
```{r}
# 순서형이 아니기에, factor형으로 변환하겠다.
all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)
sum(table(all$RoofStyle))
```

** RoofMatl: 지붕 재료 **

     ClyTile  Clay or Tile
     CompShg  Standard (Composite) Shingle
     Membran  Membrane
     Metal    Metal
     Roll Roll
     Tar&Grv  Gravel & Tar
     WdShake  Wood Shakes
     WdShngl  Wood Shingles
   
```{r}
# 순서형이 아니기에, factor형으로 변환하겠다.
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)
sum(table(all$RoofMatl))
```


###5.3.4 Land

 부지가 평지인지, 비탈인지 2개의 변수가 있다.

** LandContour: 부지의 평탄함 **

     Lvl  Near Flat/Level 
     Bnk  Banked - Quick and significant rise from street grade to building
     HLS  Hillside - Significant slope from side to side
     Low  Depression

```{r}
# 순서형이 아니기에, factor형으로 변환하겠다.
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)
sum(table(all$LandContour))
```

** LandSlope: 부지의 경사(비탈) **

     Gtl  Gentle slope
     Mod  Moderate Slope  
     Sev  Severe Slope
   
```{r}
# 순서형 타입, 정수형으로 변환하겠다.
all$LandSlope <- as.integer(revalue(all$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))
table(all$LandSlope)
sum(table(all$LandSlope))
```

###5.3.5 Dwelling

 주거에 따른 2개의 변수가 있다.

** BldgType: 주거의 형태 **

     1Fam     Single-family Detached  
     2FmCon   Two-family Conversion; originally built as one-family dwelling
     Duplx    Duplex
     TwnhsE   Townhouse End Unit
     TwnhsI   Townhouse Inside Unit
   
 순서형 타입으로 보여, 시각화해 보겠다.(single family detached = best)

```{r}
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(BldgType), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))
```

 시각화론 알수가 없다.

```{r}
# 순서형 범주가 아니기에, factor로 변환한다.
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)
sum(table(all$BldgType))
```

** HouseStyle: 주거의 Style **

     1Story   One story
     1.5Fin   One and one-half story: 2nd level finished
     1.5Unf   One and one-half story: 2nd level unfinished
     2Story   Two story
     2.5Fin   Two and one-half story: 2nd level finished
     2.5Unf   Two and one-half story: 2nd level unfinished
     SFoyer   Split Foyer
     SLvl     Split Level
   
```{r}
# 순서형 범주가 아니기에, factor로 변환한다.
all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)
sum(table(all$HouseStyle))
```

###5.3.6 Neighborhood and Conditions

 물리적 거리, 근방의 입지에 따른 3개의 변수가 있다.

** Neighborhood: 물리적 위치는 'Ames' 시내이다. **

     Blmngtn  Bloomington Heights
     Blueste  Bluestem
     BrDale   Briardale
     BrkSide  Brookside
     ClearCr  Clear Creek
     CollgCr  College Creek
     Crawfor  Crawford
     Edwards  Edwards
     Gilbert  Gilbert
     IDOTRR   Iowa DOT and Rail Road
     MeadowV  Meadow Village
     Mitchel  Mitchell
     Names    North Ames
     NoRidge  Northridge
     NPkVill  Northpark Villa
     NridgHt  Northridge Heights
     NWAmes   Northwest Ames
     OldTown  Old Town
     SWISU    South & West of Iowa State University
     Sawyer   Sawyer
     SawyerW  Sawyer West
     Somerst  Somerset
     StoneBr  Stone Brook
     Timber   Timberland
     Veenker  Veenker
   
```{r}
# 순서형 범주가 아니기에, factor로 변환한다.
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)
sum(table(all$Neighborhood))
```

** Condition1: 근방의 다양한 조건 **

     Artery   Adjacent to arterial street
     Feedr    Adjacent to feeder street   
     Norm Normal  
     RRNn Within 200' of North-South Railroad
     RRAn Adjacent to North-South Railroad
     PosN Near positive off-site feature--park, greenbelt, etc.
     PosA Adjacent to postive off-site feature
     RRNe Within 200' of East-West Railroad
     RRAe Adjacent to East-West Railroad
   
```{r}
# 순서형 범주가 아니기에, factor로 변환한다.
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
sum(table(all$Condition1))
```

** Condition2: 근방의 다양한 조건 (2개 이상) **

     Artery   Adjacent to arterial street
     Feedr    Adjacent to feeder street   
     Norm Normal  
     RRNn Within 200' of North-South Railroad
     RRAn Adjacent to North-South Railroad
     PosN Near positive off-site feature--park, greenbelt, etc.
     PosA Adjacent to postive off-site feature
     RRNe Within 200' of East-West Railroad
     RRAe Adjacent to East-West Railroad
   
```{r}
# 순서형 범주가 아니기에, factor로 변환한다.
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)
sum(table(all$Condition2))
```

###5.3.7 Pavement of Street & Driveway

 2개의 변수가 있다.

** Street: 부지에 접한 길의 종류**

     Grvl Gravel  
     Pave Paved
   
```{r}
# 순서형 범주로, 정수형으로 변환하겠다.
all$Street <- as.integer(revalue(all$Street, c('Grvl' = 0, 'Pave' = 1)))
table(all$Street)
sum(table(all$Street))
```

** PavedDrive: 진입로의 포장 **

     Y    Paved 
     P    Partial Pavement
     N    Dirt/Gravel
   
```{r}
#순서형 범주로, 정수형으로 변환하겠다.
all$PavedDrive <- as.integer(revalue(all$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))
table(all$PavedDrive)
sum(table(all$PavedDrive))
```

** character형 변수에 대한 인코딩 변환과 설명은 여기까지다 **

##5.4 Changing some numeric variables into factors

 3개의 변수를 제외하고 모든 변수들은 결측이 없이 완벽하다. 문자형은 라벨링하여 factor형으로 변환했다.
남은 3개의 변수는 실질적으론 범주형 변수이지만, 형식은 숫자형이다.

###5.4.1 Year and Month Sold

 판매 연도에서 YearBuilt(or remodeled)의 범주는 오래된 집이 가치가 낮게 되어있다. 5년치 판매의 얘기를 해보자. 여기에는 경제 위기가 있던 해가 포함되었고, 그걸 기준으로 2007년 대비 2009년(경제 위기 이후)의 집값이 매우 낮아 보인다. 모델링 전에 YrSold 변수를 factor로 변환할것이지만, Age 변수를 만들기 위해 수치형 변수도 필요하다. 그래서 아직은 factor로 변환하지 않고 두겠다.
'Month Sold'도 정수형 변수이지만, 12월은 1월보다 좋지 않다. 따라서 MoSold는 factor로 변환하겠다.

참조) 여기에서 말하는 경제 위기는 2008년에 있었던 Subprime mortgage 사태를 얘기하는 걸로 보인다.

```{r}
str(all$YrSold)
str(all$MoSold)
all$MoSold <- as.factor(all$MoSold)
```

 예상보다 변동폭이 적지만, 금융 위기의 타격은 2007년말 부터 제대로 나타났다.
매매가는 2007년이 가장 높은 중위값을 보인 이후에 점차 감소했다. 그러나 계절별로는 더 큰 규칙이 있어 보인다.

```{r}
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(YrSold), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) + #y축 20만까지 표기 제한
  geom_hline(yintercept = 163000, linetype='dashed', color = 'red') #SalePrice 중위값

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x = MoSold, y = SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) + 
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') 

grid.arrange(ys, ms, widths = c(1,2))
```

###5.4.2 MSSubClass

** MSSubClass: 판매와 연관된 주거 타입 **

    20  1-STORY 1946 & NEWER ALL STYLES
    30  1-STORY 1945 & OLDER
    40  1-STORY W/FINISHED ATTIC ALL AGES
    45  1-1/2 STORY - UNFINISHED ALL AGES
    50  1-1/2 STORY FINISHED ALL AGES
    60  2-STORY 1946 & NEWER
    70  2-STORY 1945 & OLDER
    75  2-1/2 STORY ALL AGES
    80  SPLIT OR MULTI-LEVEL
    85  SPLIT FOYER
    90  DUPLEX - ALL STYLES AND AGES
    120  1-STORY PUD (Planned Unit Development) - 1946 & NEWER
    150  1-1/2 STORY PUD - ALL AGES
    160  2-STORY PUD - 1946 & NEWER
    180  PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
    190  2 FAMILY CONVERSION - ALL STYLES AND AGES
  
 이 클래스는 숫자로 나오지만, 실제적으로 범주형은 아니다.

```{r}
str(all$MSSubClass)

all$MSSubClass <- as.factor(all$MSSubClass)

# 가독성을 높이기 위해 숫자를 문자로 revalue
all$MSSubClass <- revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

str(all$MSSubClass)
```

#6 Visualization of important variables

 끝이 보인다. 모든 문자형 변수를 범주형 factor나, 라벨링하여 숫자형으로 인코딩했다.
더해서, 3개의 수치형 변수는 factor로 변환했고, 1개 변수(Utilities)는 삭제했다.
아래와 같이, 수치형 변수의 수는 이제 56개(종속 변수 포함), 남은 23개 변수는 범주형이다.

```{r}
numericVars <- which(sapply(all, is.numeric)) # index vector numeric variables
factorVars <- which(sapply(all, is.factor))   # index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 
    'categoric variables')
```

##6.1 Correlations again

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

##6.2 Finding variable importance with a quick Random Forest

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

###6.2.1 Above Ground Living Area, and other surface related variables (in square feet)

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

###6.2.2 The most important categorical variables; Neighborhood

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

###6.2.3 Overall Quality, and other Quality variables

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

###6.2.4 The second most important categoriacal variables; MSSubClass

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

###6.2.5 Garage variables

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

###6.2.6 Basement variables

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

#7 Feature engineering

##7.1 Total number of Bathrooms

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

##7.2 Adding 'House Age', 'Remodeled (Yes/No)', and IsNew Variables

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

##7.3 Binning Neighborhood

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

##7.4 Total Square Feet

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

##7.5 Consolidating Porch variables

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
