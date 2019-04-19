---
title: "Kaggle House Prices Prediction Competition with R (한글 번역) EDA & FE [4]"
author: "Creed Maestro"
date: "2019/03/23"
categories: Kaggle
tags: Kaggle, R, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github

---


[캐글 Rmd 링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)


## 5.3 Label encoding/ factorizing the ramaining character variables {.tabset}

 결측치는 다 살펴보았지만, character 타입이었던 변수들은 아직 확인이 되지 않았다.
그래서 이번에도 탭으로 구분하여 살펴 보겠다.

```{r}
Charcol <- names(all[,sapply(all, is.character)]) #문자형 변수만 선별하여 생성
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')
```

### 5.3.1 Foundation

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


### 5.3.2 Heating and airco

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


### 5.3.3 Roof

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


### 5.3.4 Land

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

### 5.3.5 Dwelling

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

### 5.3.6 Neighborhood and Conditions

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

### 5.3.7 Pavement of Street & Driveway

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

## 5.4 Changing some numeric variables into factors

 3개의 변수를 제외하고 모든 변수들은 결측이 없이 완벽하다. 문자형은 라벨링하여 factor형으로 변환했다.
남은 3개의 변수는 실질적으론 범주형 변수이지만, 형식은 숫자형이다.

### 5.4.1 Year and Month Sold

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

### 5.4.2 MSSubClass

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
