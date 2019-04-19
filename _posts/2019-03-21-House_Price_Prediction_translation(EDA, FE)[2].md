---
title: "Kaggle House Prices Prediction Competition with R (한글 번역) EDA & FE [2]"
author: "Creed Maestro"
date: "2019/03/21"
categories: Kaggle
tags: Kaggle, R, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github

---


[캐글 Rmd 링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)



### 5.2.8 Basement variables

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


### 5.2.9 Masonry variables

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
# veneer type의 결측치를 최빈도값으로 대체하자
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


### 5.2.10 MS Zoning

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


### 5.2.11 Kitchen variables

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


