---
title: "Kaggle House Prices Prediction Competition with R (한글 번역) EDA & FE [3]"
author: "Creed Maestro"
date: "2019/03/22"
categories: Kaggle
tags: Kaggle, R, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github

---


[캐글 Rmd 링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)

### 5.2.12 Utilities

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

### 5.2.13 Home functionality
 
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



### 5.2.14 Exterior variables

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


### 5.2.15 Electrical system

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


### 5.2.16 SaleType and Condition

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
