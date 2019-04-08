---
title: "House Prices Prediction with R (한글 번역) Modeling"
author: "Maestro"
date: "2019/03/21"
categories: Kaggle
tags: Kaggle
layout: post
output: 
  md_document:
    variant: markdown_github
    
---

#8 Preparing data for modeling

##8.1 Dropping highly correlated variables

 상관 관계가 높은 두 변수가 있다면 한 변수를 삭제하겠다. 상관 관계 짝을 찾기 위해, 섹션 6.1에서 다룬 상관 행렬을 쓰겠다. 예를 들어 'Garage Cars' & 'GarageArea' 의 상관 계수가 0.89이다. 둘 중 판매가와 상관 관계가 낮은 변수 하나를 지우겠다. ('GarageArea'와 'SalePrice'의 상관 계수 0.62, "GarageCars"와 'SalePrice'의 상관 계수 0.64)

```{r}
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]
```

##8.2 Removing outliers

 낮은 판매가 대비 면적이 컸던 두 채의 집을 이상치로 잡고 제거하겠다. 그러나, 후에 자세히 조사하겠다.
('outliers' 패키지를 사용해서)

```{r}
all <- all[-c(524, 1299), ]
```

##8.3 PreProcessing predicor variables

 모델링 전에 라벨링 인코딩하지 않은 숫자형 독립 변수를 조정하겠다. 
그리고 범주형 독립 변수를 위해 더미 변수를 만들겠다. 
모든 숫자형 독립 변수와 범주형 변수를 데이터프레임으로 분할하겠다.

```{r}
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames 작업 완료전 변수 생성
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')
```

###8.3.1 Skewness and normalizing of the numeric predictors

 Skewness(왜곡)는 분포에서 대칭을 측정한 것이다. 대칭 데이터셋의 왜곡은 0이다. 
그래서, 정규 분포는 왜곡이 0일 것이다. 왜곡은 원래 양측 꼬리의 상대적 크기를 측정한다.
경험상, 왜곡은 -1과 1 사이이다. 이 범위에서, 데이터는 상당히 대칭적인 것으로 간주된다.
왜곡을 수정하려면, 모든 숫자형 독립 변수와 0.8(실질적으로 log+1, 제로 이슈로 인한 분열을 피하기 위해)보다 큰 absolute skew의 로그를 가져와야 한다.

```{r}
for(i in 1:ncol(DFnumeric)){
        if (abs(skew(DFnumeric[,i])) > 0.8){
                DFnumeric[,i] <- log(DFnumeric[,i] +1)
        }
}
```

** Normalizing the data **

```{r}
PreNum <- preProcess(DFnumeric, method=c('center', 'scale')) #'center', 'scale' preProcessing후 변수 생성
print(PreNum)
```
```{r}
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
```

###8.3.2 One hot encoding the categorical variables

 마지막으로, 최상의 머신러닝 알고리즘을 위해 모든 독립 변수를 'one-hot encode' 하여 범주형 변수인 숫자 컬럼으로 변환하겠다. 즉, model.matrix()함수를 써서 범주형 값을 1과 0으로 컬럼을 분리하겠다.(1은 Yes) 

```{r}
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
```

###8.3.3 Removing levels with few or no observation in train or test

 이전 버전에서, 'Caret'패키지의 'Near Zero Variance' 함수를 썻다. 이건 빠르지만, 정보의 손실이 너무 크다.
이걸 기본으로 설정하여 'one-hot encode'하면 모든 동네에서 최소 146채의 주택 변수가 생략된다.
따라서, 이 버전에서 조심스러운 접근법을 택했다.

```{r}
# test셋에 없는 변수가 있는지 확인
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] # 예측 변수 제거
```

```{r}
# train셋에 없는 변수가 있는지 확인
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain] #예측 변수 제거
```

 train 셋에서 10 미만의 값을 갖는 변수를 추출

```{r}
fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),]) < 10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #예측 변수 제거
dim(DFdummies)
```

 변수가 적거나 없는 독립 변수 49개를 'one-hot encode'하여 지우겠다. 개수가 많아 보이지만, 실제로는 caret's의 near zero variance 함수를 써서 지운 변수보다 개수가 적다.

```{r}
combined <- cbind(DFnorm, DFdummies) # 컬럼 결합으로 데이터프레임 생성
```

##8.4 Dealing with skewness of response variable

```{r}
skew(all$SalePrice)
```

```{r}
qqnorm(all$SalePrice)
qqline(all$SalePrice) 
```

 1.87의 skew는 우편향으로 아주 높은 기울기이다. Q-Q plot의 판매가는 정규 분포가 아님을 보여준다.
판매가를 log 변환해서 조정하겠다.

```{r}
all$SalePrice <- log(all$SalePrice) # default is the natural logarithm
skew(all$SalePrice)
```

보는 바와 같이, skew가 낮아졌고, Q-Q plot도 좋아졌다.

```{r}
qqnorm(all$SalePrice)
qqline(all$SalePrice)
```

##8.5 Composing train and test sets

```{r}
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]
```

#9 Modeling

##9.1 Lasso regression model

 Ridge, Elastic Net model, Lasso 3가지 모델을 시도했는데 Lasso가 제일 나은 결과라서 Lasso만 적겠다.
elastic-net 패널티는 알파와 Lasso(alpha = 1) & ridge(alpha = 0)간의 갭 사이에서 조정된다.
튜닝값 Lambda는 전반적인 패널티의 strength를 조정한다. 
Ridge 패널티는 독립 변수의 상관 계수를 줄이는 걸로 알려졌지만, Lasso는 하나는 택하고 다른건 버리는 걸로 알려졌다. 
아래와 같이 caret 함수를 교차 검증하여 lambda에 쓸 best 값을 찾아냈고, 이건 lasso model에서 조정해야할 유일한 매개 변수다.

```{r}
set.seed(27042018)
my_control <- trainControl(method = 'cv', number = 5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0005))

lasso_mod <- train(x = train1, y = all$SalePrice[!is.na(all$SalePrice)], method ='glmnet', trControl= my_control, tuneGrid = lassoGrid)
lasso_mod$bestTune
min(lasso_mod$results$RMSE)
```

 caret의 'varlmp' 함수에는 다음과 같이 쓰여 있다. glmboost와 glmnet의 완벽한 상관 계수의 값을 위해 이에 대응하는 튜닝된 모델이  사용된다. 이것은 가장 중요한 변수의 실제 순위가 저장되지 않는다는 거지만, 이로 인해 모델에서 얼마나 많은 변수가 미사용 되는지를 알 수 있게 해준다.

```{r}
lassoVarImp <- varImp(lasso_mod, scale = F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
```

 lasso가 제안한 작업을 했음: 모델에서 사용 가능한 변수의 약 45%를 미사용해서 다중공선성을 잘 처리한 것으로 보인다.

```{r}
LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #로그를 실수로 변환.
head(predictions_lasso)
```

##9.2 XGBoost model

 처음에는 XGBoost 패키지로 직접 작업했다. 'xgb.Dmatrix'라는 그것의 특별한 데이터 구조를 쓰기 위해서였다.
이 패키지 역시 교차 검증을 지원한다.
그러나, 이 교차 검증 함수는 최적 라운드 수를 결정하며 매개 변수의 full grid search를 지원하지 않는다.
caret은 xgb 패키지의 (빠른) 데이터 구조를 사용하지 않는 것 같지만, 최소한 full grid search를 제공하기 위해 어떻게든 매개 변수를 튜닝 하기로 했다. 내가 아는 한, 과적합을 피하기 위해 'max_depth', 'min_child_weight' 메인 변수를 튜닝해야 한다. 아래와 같이 이 변수와 기타(learning rate) 변수들을 튜닝했다.

```{r}
xgb_grid = expand.grid(nrounds = 1000,
                       eta = c(0.1, 0.05, 0.01),
                       max_depth = c(2, 3, 4, 5, 6),
                       gamma = 0, 
                       colsample_bytree = 1,
                       min_child_weight = c(1, 2, 3, 4, 5),
                       subsample = 1
                       )
```

 다음으론 최적의 매개 변수를 찾는다 (5 fold cross validation)

```{r}
#xgb_caret <- train(x = train1, y = all$SalePrice[!is.na(all$SalePrice)], method = 'xgbTree', trControl = my_control, tuneGrid = xgb_grid)
#xgb_caret$bestTune
```

 예상한대로, 많은 시간이 걸렸다. Kaggle에서 실행 시간 제한을 위해, 코드를 비활성화하고, 결과를 계속 보겠다.
Caret의 'BestTune' 변수:

* Max_depth=3
* eta=0.05
* Min_child_weight=4

 섹션의 나머지 부분에서 xgboost 패키지를 직접 작업 할 것이다. 추천 포맷의 데이터를 준비하기 위해 시작할 것이다.

```{r}
label_train <- all$SalePrice[!is.na(all$SalePrice)]

# Dmatrix로 testing & training data 분리
dtrain <- xgb.DMatrix(data = as.matrix(train1), label = label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))
```

 게다가, caret cross validaion에서 최적의 값을 선별하겠다.

```{r}
default_param <- list(
  objective = 'reg:linear',
  booster = 'gbtree',
  eat = 0.05, # default = 0.3,
  gamma = 0,
  max_depth = 3, # default = 6
  min_child_weight = 4, # default = 1,
  subsample = 1, 
  colsample_bytree = 1
)
```

 다음으로 최적의 라운드 수를 결정하기 위한 교차 검증(cross validation)을 하겠다(주어진 변수 셋에 대해)

```{r}
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
```

 간략한 작업 이었지만, 교차 검증 된 RMSE가 상당히 향상되었기에 매개 변수 튜닝을 확실하게 했다.
(0.1225에서 0.1162로 되었다.)

```{r}
# 교차 검증으로 찾은 최적의 iteration을 써서 모델 트레이닝
xgb_mod <- xgb.train(data = dtrain, parmas = default_param, nrounds = 454)
```

```{r}
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) # 로그를 실수로 변환
head(predictions_XGB)
```

```{r}
# 변수 중요도 시각화
library(Ckmeans.1d.dp) # ggplot clustering을 위해 필요하다
mat <- xgb.importance(feature_names = colnames(train1), model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)
```

##9.3 Averaging predictions

 Lasso와 XGBoost의 알고리즘이 매우 달라서 평균 예측으로 점수가 향상 될 수 있다.
Lasso 모델이 교차 검증된 RMSE 점수(0.1121 versus 0.1162)가 더 좋으므로, lasso model에 weighting을 2배로 하겠다.

```{r}
sub_avg <- data.frame(ID = test_labels, SalePrice = (predictions_XGB + 2*predictions_lasso)/3)
head(sub_avg)
write.csv(sub_avg, file = 'average.csv', row.names = F)
```

---


#10 Ref.

원문: https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/report
원저자: Erik Bruin

#11 Kaggle 커널 링크

[캐글링크](https://www.kaggle.com/maestroyi/house-prices-prediction-with-r-to-korean/report?scriptVersionId=12633146)