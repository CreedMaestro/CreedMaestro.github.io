---
title: "캐글코리아 2nd Competition House Price Prediction with R (시애틀 집값 예측) Modeling"
author: "Maestro"
date: "2019/04/15"
categories: Project
tags: Kaggle, 캐글, 캐글코리아, 집값예측
layout: post
output: 
  md_document:
    variant: markdown_github
---


# 6 Preparing data for modeling
  참조 & 필사한 링크를 첨부한다. 'erikbruin'의 링크를 참조하다 에러가 나는 부분이 많아, 아래 'psystat'님의 커널을 필사했다.
 들어가는 변수가 달라 점수는 차이가 많이 난다. 최적화 parameter를 찾아서 모델을 손보려 했는데 이 부분은 통계와 수학의 기반 지식이 필요해서 지금은 불가능했다.
 이번 컴퍼티션은 EDA와 FE까지를 주로 보고, 모델링은 대략적인 필사로 했다.
 

https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda

https://www.kaggle.com/psystat/eda-and-lasso-rf-svm-xgb-grid-search/code


## 6.1 Removing Outliers
```{r}
outlier <- k_all[c(2776,5109,8913,13810,7247,3135), c('price','city','sqft_living','sqft_lot','living_divide_lot')]
outlier
#2776, 5109는 대조군으로 두고 면적 대비 가격이 낮았던 '8913',' '13810', '7247'과 view가 0등급인데 가격이 높았던 '3135'를 이상치로 정하고 데이터를 확인해 보겠다.
```

대조군 대비 '8913', '13810'의 부지당 평단가가 너무 낮다. 대조군이 100단위인 반면, 이 두 이상치는 1,2자리이다.
이상치로 잡고 제거하겠다.

```{r}
k_all <- k_all[-c(8913,13810),]
```

## 6.2 preprocessing
```{r}
k_all[, Yrmbuy:=factor(Yrmbuy)]
k_all[, zipcode:=factor(zipcode)]
k_all[, city:=factor(city)]
k_all[, mecity:=factor(mecity)]
k_all[, waterfront:=factor(waterfront)]
k_all[, IsNew:=factor(IsNew)]
k_all[, Remod:=factor(Remod)]
k_all[, view:=factor(view)]
k_all[, grade:=factor(grade)]
k_all[, floors:=factor(floors)]
k_all[, TotalRooms:=factor(TotalRooms)]
k_all[, condition:=factor(condition)]

cat_vars <- c('waterfront', 'Yrmbuy','zipcode','mecity','city','IsNew','condition','Remod','view','grade','floors','TotalRooms') # 가져갈 변수
del_vars <- c('price','Yrbuy','Mobuy','date', 'yr_built','yr_renovated') #제거할 변수
num_vars <- setdiff(colnames(k_all), c(cat_vars, del_vars))  #del_var 제외 변수 추출


# 수치형 변수 표준화
X_train_num <- k_all[!is.na(k_all$price), num_vars, with=F]
X_test_num <- k_all[is.na(k_all$price), num_vars, with=F]

mean.tr <- apply(X_train_num, 2, mean)
sd.tr <- apply(X_train_num, 2, sd)

X_train_num <- scale(X_train_num, center = mean.tr, scale = sd.tr)
X_test_num <- scale(X_test_num, center = mean.tr, scale = sd.tr)

X_train <- model.matrix(~.-1, data=cbind(X_train_num, k_all[!is.na(k_all$price), cat_vars, with=F]))
X_test <- model.matrix(~.-1, data = cbind(X_test_num, k_all[is.na(k_all$price), cat_vars, with=F]))
Y_train <- log(k_all[!is.na(k_all$price), price])
```

# 7 Modeling

## 7.1 LASSO
```{r}
RMSE_exp <- function (data, lev = NULL, model = NULL) {
  out <- sqrt(mean((exp(data$obs) - exp(data$pred))^2))
  names(out) <- "RMSE_exp"
  out
}

tic('LASSO')
set.seed(0418)
k_control <- trainControl(method = 'cv', number = 5, summaryFunction = RMSE_exp)
tuneGrid <- expand.grid(alpha = 1, lambda = c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1))
fit.lasso <- train(x = as.matrix(X_train), y= Y_train,
                   method = 'glmnet', 
                   metric = 'RMSE',
                   maximize = FALSE,
                   trControl = k_control,
                   tuneGrid = tuneGrid)
```

```{r}
toc()

fit.lasso

fit.lasso$bestTune

min(fit.lasso$results$RMSE_exp)
```

```{r}
lasso.varimp <- varImp(fit.lasso, scale = F)
lasso.imp <- lasso.varimp$importance
lasso.importance <- data.frame(variable = rownames(lasso.imp), importance = lasso.imp$Overall) %>% 
  arrange((importance))
lasso.importance$variable <- factor(lasso.importance$variable, levels = lasso.importance$variable, labels = lasso.importance$variable)

num.vars.selected <- length(which(lasso.importance$importance!=0))
num.vars.not.selected <- length(which(lasso.importance$importance==0))
cat('Lasso uses', num.vars.selected, 'variables in its model, and did not select', num.vars.not.selected, 'variables')

lasso.importance[which(lasso.importance$importance!=0),] %>% 
  ggplot(aes(x=variable, y=importance, fill=variable)) + 
                            geom_bar(alpha=0.5, stat='identity') + 
                            coord_flip() + # 가로 barplot
                            theme_light() + theme(legend.position = "none")
```

```{r}
as.character(lasso.importance$variable[which(lasso.importance$importance==0)])

predictions_lasso <- exp(predict(fit.lasso, X_test))
head(predictions_lasso)

submission_lasso <- read.csv('../input/sample_submission.csv')
submission_lasso$price <- predictions_lasso
write.csv(submission_lasso, file = 'submission_lasso.csv', row.names = F)
```

## 7.2 XGBoost

```{r}
tuneGrid <- expand.grid(
  max_depth = c(6, 60),  #default: 6
  subsample = c(0.8, 1), #default: 1
  colsample_bytree = c(0.9, 1) #default: 1
)

RMSE_exp <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, 'label')
  err <- sqrt(mean((exp(labels) -exp(preds))^2))
  return(list(metric = 'RMSE_exp', value = err))
}

# Dmatrix로 testing & training data 분리
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))
results <- list(val_rmse = rep(0, nrow(tuneGrid)),
                nrounds = rep(0, nrow(tuneGrid)))

for ( i in 1:nrow(tuneGrid)){
  params <- list(
    objective = 'reg:linear',
    metric = 'rmse',
    booster = 'gbtree',
    eta = 0.01, #default: 0.3
    gamma = 0,  #default: 0
    min_child_weight =1, #default: 1
    max_depth = tuneGrid[i, 'max_depth'],
    subsample = tuneGrid[i, 'subsample'],
    colsample_bytree = tuneGrid[i, 'colsample_bytree']
  )
}
```

** cross validation **
```{r}
# tic('xgbcv')
xgbcv <- xgb.cv(params = params, 
                   data = dtrain, 
                   nrounds = 10000, 
                   nfold = 5,
                   feval = RMSE_exp,
                   print_every_n = 100,
                   early_stopping_rounds = 100,
                   maximize = F,
                   seed=4018)

# toc()
results[['val_rmse']][i] <- unlist(xgbcv$evaluation_log[xgbcv$best_iteration, 'test_RMSE_exp_mean'])
results[['nrounds']][i] <- xgbcv$best_iteration
```

```{r}
min.index <- which.min(results[['val_rmse']])
tuneGrid[min.index,]
cbind(tuneGrid, RMSE = unlist(results[['val_rmse']]))
```

```{r}
default_param <- list(objective = 'reg:linear',
                        booster = 'gbtree',
                        eta = 0.01,
                        gamma = 0,
                        min_child_weight = 1,
                        max_depth         = tuneGrid[min.index, "max_depth"],
                        subsample         = tuneGrid[min.index, "subsample"],
                        colsample_bytree  = tuneGrid[min.index, "colsample_bytree"]
                        )

fit.xgb <- xgb.train(data=dtrain,
                     params = default_param,
                     nrounds = results[['nrounds']][min.index],
                     seed = 4018)

predictions_xgb <- exp(predict(fit.xgb, dtest))
head(predictions_xgb)
```
```{r}
submission_xgb <- read.csv('../input/sample_submission.csv')
submission_xgb$price <- predictions_xgb
write.csv(submission_xgb, file = 'submission_xgb.csv', row.names = F)
```

## Ensemble
 Lasso와 XGBoost의 결과값 앙상블
```{r}
k_sub.avg <- data.frame(ID=k_test.labels, price = (2*predictions_xgb + predictions_lasso)/3)
head(k_sub.avg)
write.csv(k_sub.avg, file = 'average.csv', row.names = F)
```

[캐글 RMD Report 링크]
("https://www.kaggle.com/maestroyi/house-price-prediction-with-r-eda-fe/report")

# 소감

 필사가 아닌 첫 컴퍼티션이었다. EDA와 FE는 여태까지 공부해왔던 R로 할 수 있었지만, 모델링 부분은 R 이외에 통계와 수학의 기반 지식이 필요했다. 데이터쪽 공부를 할 수록 이 분야는 한 사람이 다 하는 건 무리가 아닐까 하는 생각이 커간다. 
 Data Scientist는 통계와 수학을 Base로 모델 개발을 하여 업계 사람들과 대화를 하고, 
 Data Analyst는 도메인과 커뮤니케이션 능력을 기반으로 비전공자와 의사결정자에게 Data를 풀어 인사이트를 보여주고,
 Data Engineer는 로그데이터를 추출하여 DB를 구축하고 유지. 보수를 하고 
 각 각의 포지션마다 필요한 역량이 다르고, 그 역량도 깊이 있게 알아야 하기에 데이터 과학자로 다 묶는게 아니라 세분화가 된 것 같다.
 왜 회사에서 Data Scientist를 신입으로 안 뽑는지 알게 된 계기였다. 이건 신입이 하기엔 부하가 많이 가고, 사소한 실수가 생겨 경력직들이 커버를 하기엔 리스크가 너무 크다. 
  Data를 다룰 줄 아는 Business Analyst. 도메인을 가진 사람들이 Data 업계로 넘어가기엔 이 포지션이 맞아 보인다.
 