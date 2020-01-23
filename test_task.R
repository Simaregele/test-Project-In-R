# test application for ChemTech AI 
setwd("C:/Users/mrsim/Google Диск/R statistic/test_task")
library(ggplot2)
library(psych)
library(caTools)
library(caret)
library(ggpubr) # for multiplying plots dont work with qqplot
library(rapport)


train_data  = read.csv("train_data_200k.csv")
test_data = read.csv("test_data_100k.csv")


##### 1 model, del date, del cor, del feature with less 70% data

#del date 
train_data <- subset(train_data, select = -c(X))


# del cor tag
cor_data <- function(dataset){
  cor_data <- cor(dataset, use = "pairwise.complete.obs") # or complete.obs
  cor_data
}

# delete coll that have less 70% of values
train_data <- train_data[,sapply(train_data, function(x) (sum(is.na(x))/2000))<0.70]


## prepare for norm
temp_targets <- subset(train_data, select = c(target1, target2, target3, target4))
temp_data <- subset(train_data, select = -c(target1, target2, target3, target4))
temp_data <- sapply(temp_data, scale)
temp_data <- as.data.frame(temp_data)
train_data <- subset(temp_data)

# correlation matrix
correlation_data <- cor_data(train_data)
### fin correlation
corr_indexes <- findCorrelation(correlation_data, cutoff = 0.7)

data_with_corr <- subset(train_data, select = c(corr_indexes))
train_data <- subset(train_data, select = c(corr_indexes))
train_data <- subset(train_data, select=-c(target1))


train_data$target1 <- temp_targets$target1
train_data$target2 <- temp_targets$target2
train_data$target3 <- temp_targets$target3
train_data$target4 <- temp_targets$target4


## split the data
### Train, validate, test
set.seed(145)
sample = sample.split(train_data, SplitRatio = 0.60)
target1_train =subset(train_data,sample ==TRUE)
target1_val =subset(train_data,sample ==FALSE)
sample2 = sample.split(target1_val, SplitRatio = 0.50)
target1_test =subset(target1_val,sample ==FALSE)
target1_val =subset(target1_val,sample ==TRUE)


# own lossfunctions
mse <- function(true, pred){
  val_one <- (true - pred)**2
  sum_of_val <- sum(val_one, na.rm = T)
  return(sum_of_val)}

mae <- function(true, pred){
  val_one <- abs(true - pred)
  sum_of_val <- sum(val_one, na.rm = T)
  return(sum_of_val)}


# set.seed(145)
# sample = sample.split(train_data, SplitRatio = 0.70)
# target1_train =subset(train_data_r,sample ==TRUE)
# target1_test =subset(train_data_r,sample ==FALSE)
# # random forest, lets try
# train_data.rf <- randomForest(target1~., train_data, na.rm=T)


#lm on all features dont drop corr values
# for target 1
# train_data_r <- subset(train_data, select = -c(target2, target3, target4))
# 
# set.seed(145)
# sample = sample.split(train_data_r, SplitRatio = 0.70)
# target1_train =subset(train_data_r,sample ==TRUE)
# target1_test =subset(train_data_r,sample ==FALSE)

### fin correlation
corr_indexes <- findCorrelation(correlation_data, cutoff = 0.7)


fullmodel <- lm(target1 ~ ., data = na.omit(target1_train))
one_tag_nodel <- lm(target1 ~ tag1, data = na.omit(target1_train))
summary(one_tag_nodel)
step_model <- step(fullmodel, direction = "backward")
hist(train_data$target4)
shapiro.test(train_data$target1)

## dont work wirh normalized
step_fited_model <- lm(formula = target1 ~ (log(tag10) + tag11 + tag12 + 
                                          tag13 + tag21 + 
                                          tag23 + tag24 + tag27 + tag28 + tag30 + 
                                          tag31 + tag32 + tag35 + tag42 + tag43 + tag47 + 
                                          tag49 + tag55 + tag58  + tag63 + tag64 + log(tag65) + 
                                          tag66 + log(tag67) + log(tag70) + log(tag71) + tag74 + tag75 + 
                                          log(tag76))**2
                         , data = na.omit(target1_train))
summary(step_fited_model)
step_model <- step(step_fited_model, direction = "backward")
# Residual standard error: 1.584e-05 on 38747 degrees of freedom
# (98900 observations deleted due to missingness)
# Multiple R-squared:   0.97,	Adjusted R-squared:  0.9697 
# F-statistic:  2879 on 435 and 38747 DF,  p-value: < 2.2e-16
step_fited_model <- lm(formula = target1 ~ (tag10 + tag11 + tag12 + 
                                              tag13 + tag21 + 
                                              tag23 + tag24 + tag27 + tag28 + tag30 + 
                                              tag31 + tag32 + tag35 + tag42 + tag43 + tag47 + 
                                              tag49 + tag55 + tag58  + tag63 + tag64 + tag65 + 
                                              tag66 + tag67 + tag70 + tag71 + tag74 + tag75 + 
                                              tag76)**2
                       , data = na.omit(target1_train))
summary(step_fited_model)
# Residual standard error: 1.829e-05 on 117348 degrees of freedom
# Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9543 
# F-statistic:  5652 on 435 and 117348 DF,  p-value: < 2.2e-16

#lets predict
target1_test$predict <- predict(step_fited_model, target1_test)
target1_val$predict <- predict(step_fited_model, target1_val)
# test
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)
# > mse(target1_test$target1, target1_test$predict)
# [1] 1.182325e-05
# > mae(target1_test$target1, target1_test$predict)
# [1] 0.4433596
# val
mse(target1_val$target1, target1_val$predict)
mae(target1_val$target1, target1_val$predict)
# [1] 3.244601e-05
# > mae(target1_val$target1, target1_val$predict)
# [1] 0.6599488

train_data <- subset(train_data, select = -c(target1))
train_data$target2 <- temp_targets$target2

# predict test_data
test_data$target1 <- predict(step_fited_model, test_data)



write.csv(test_data,'test_data_my_pred.csv')


# rf
train_data.rf <- randomForest(target1 ~ (tag10 + tag11 + tag12 + 
                                           tag13 + tag21 + 
                                           tag23 + tag24 + tag27 + tag28 + tag30 + 
                                           tag31 + tag32 + tag35 + tag42 + tag43 + tag47 + 
                                           tag49 + tag55 + tag58  + tag63 + tag64 + tag65 + 
                                           tag66 + tag67 + tag70 + tag71 + tag74 + tag75 + 
                                           tag76),data = na.omit(target1_train))
summary(train_data.rf)






## neural yeah! 2 layers
library(neuralnet)
neural <- neuralnet(target1 ~ ., hidden=2, data = na.omit(target1_train))
plot(neural)
target1_test$predict <- predict(neural, target1_test)
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)
# > mse(target1_test$target1, target1_test$predict)
# [1] 0.0004451295
# > mae(target1_test$target1, target1_test$predict)
# [1] 3.965331
# bad result

# 3 layers
neural <- neuralnet(target1 ~ ., hidden=3, data = na.omit(target1_train))
target1_test$predict <- predict(neural, target1_test)
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)
# > mse(target1_test$target1, target1_test$predict)
# [1] 0.0004451121
# > mae(target1_test$target1, target1_test$predict)
# [1] 3.966327

# good values?
neural <- neuralnet(target1 ~ (tag10 + tag11 + tag12 + 
                                 tag13 + tag21 + 
                                 tag23 + tag24 + tag27 + tag28 + tag30 + 
                                 tag31 + tag32 + tag35 + tag42 + tag43 + tag47 + 
                                 tag49 + tag55 + tag58  + tag63 + tag64 + tag65 + 
                                 tag66 + tag67 + tag70 + tag71 + tag74 + tag75 + 
                                 tag76), hidden=3, data = na.omit(target1_train))
target1_test$predict <- predict(neural, target1_test)
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)
# [1] 0.0004468171
# > mae(target1_test$target1, target1_test$predict)
# [1] 3.98364



# cor values
corr_data <- cor_data(train_data)
which(cor_data>0.5&cor_data!=1, arr.ind = TRUE)


sort_data <- which(cor_data>0.5&cor_data!=1, arr.ind = TRUE)

#find and delecte corr values
corr_index <- findCorrelation(corr_data, cutoff = 0.50, verbose = T)
train_data_r <- subset(train_data, select = -c(corr_index))

# add some data back
train_data_r$target2 <- train_data$target2
train_data_r$target1 <- train_data$target1

#### 
#lets build a qqplots for targer1 / target2
str(train_data_r)
# target 1 plots
tg1_t1 <- qqplot(train_data_r$tag1, train_data_r$target1) # tag1 maybe log? looks like exp
tg1_t12 <- qqplot(train_data_r$tag12, train_data_r$target1) # with step
tg1_t20 <- qqplot(train_data_r$tag20, train_data_r$target1) # maybe i should log it
tg1_t22 <- qqplot(train_data_r$tag22, train_data_r$target1) # anothet about linear graphick
tg1_t23 <- qqplot(train_data_r$tag23, train_data_r$target1) # with step like tag12 
tg1_t24 <- qqplot(train_data_r$tag24, train_data_r$target1) # another step hmm
tg1_t43 <- qqplot(train_data_r$tag43, train_data_r$target1) # exp graph hmm
tg1_t55 <- qqplot(train_data_r$tag55, train_data_r$target1) # exp graph
tg1_t56 <- qqplot(train_data_r$tag56, train_data_r$target1) # exp graph
tg1_t58 <- qqplot(train_data_r$tag58, train_data_r$target1) # wow whatis thaat??
tg1_t60 <- qqplot(train_data_r$tag60, train_data_r$target1) # super exp
tg1_t63 <- qqplot(train_data_r$tag63, train_data_r$target1) # exp 
tg1_t75 <- qqplot(train_data_r$tag75, train_data_r$target1) # exp

# split data, regression -> mse
set.seed(145)
sample = sample.split(train_data_r, SplitRatio = 0.70)
target1_train =subset(train_data_r,sample ==TRUE)
target1_test =subset(train_data_r,sample ==FALSE)
target1_train <- subset(target1_train, select = -c(target2, target3, target4))
target1_test <- subset(target1_test, select = -c(target2, target3, target4))
str(target1_test)
str(target1_train)


fit_tg1 <- lm(target1 ~ (tag1+tag12+tag20+tag22+tag23+tag24+tag43
                         +tag55+tag56+tag58+tag60+tag63+tag75)+(tag1+tag12+tag20+tag22+tag23+tag24+tag43
              +tag55+tag56+tag58+tag60+tag63+tag75)**2+(tag1+tag12+tag20+tag22+tag23+tag24+tag43
                                                        +tag55+tag56+tag58+tag60+tag63+tag75)**3
              , target1_train, na.action = na.omit)
summary(fit_tg1)
target1_test$predict <- predict(fit_tg1, target1_test)
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)
# Adjusted R-squared:  0.5636 



# bad model lets see corr
cor_data(target1_train)
target1_test$predict <- predict(fit_tg1, target1_test)




sum(is.na(target1_test_predict))

sum(target1_test$target1[target1_test$target1==""])
   
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)

length(target1_test[which(is.na(target1_test_predict)),])

#### target1
#### mse - 0.000225491
#### mae - 2.93818

# mse return na... maybe we could fill na data with mean?
# ok lets try ro fill it with meab
# UPD NA in predict
target1_test$target1[is.na(target1_test$target1)] <- mean(target1_test$target1, na.rm = T)



fit2_tg1 <- lm(target1 ~ tag1+tag12+tag20+log(tag22)+tag23+tag24+tag43
               +tag55+tag56+log(tag58)+tag60+tag63+log(tag75), target1_train, na.action = na.omit)
summary(fit2_tg1)

target1_test$predict <- predict(fit2_tg1, target1_test)
mse(target1_test$target1, target1_test$predict)
mae(target1_test$target1, target1_test$predict)

anova(fit_tg1, fit2_tg1)


#### target2 i thik same data as target1

####target 3



# split data, regression -> mse


train_data_r <- subset(train_data, select = -c(target2, target1, target4))
set.seed(145)
sample = sample.split(train_data_r, SplitRatio = 0.70)
target3_train =subset(train_data_r,sample ==TRUE)
target3_test =subset(train_data_r,sample ==FALSE)
target3_train <- subset(target3_train, select = -c(target2, target1, target4))
target3_test <- subset(target3_test, select = -c(target2, target1, target4))
str(target3_train)
str(target3_test)

full_tg3 <- lm(target3 ~ ., data = na.omit(target3_train))
summary(full_tg3)
step_big
fit_tg3 <- lm(target3 ~ tag1+tag12+tag20+tag22+tag23+tag24+tag43
              +tag55+tag56+tag58+tag60+tag63+tag75, target3_train, na.action = na.omit)
summary(fit_tg3)
# Adjusted R-squared:  0.4752 


target3_test$predict <- predict(fit_tg3, target3_test)

mse(target3_test$target3, target3_test$predict)
mae(target3_test$target3, target3_test$predict)

# > mse(target3_test$target3, target3_test$predict)
# [1] 0.5747649
# > mae(target3_test$target3, target3_test$predict)
# [1] 151.9379




####target 4

# split data, regression -> mse
set.seed(145)
sample = sample.split(train_data_r, SplitRatio = 0.70)
target4_train =subset(train_data_r,sample ==TRUE)
target4_test =subset(train_data_r,sample ==FALSE)
target4_train <- subset(target4_train, select = -c(target2, target1, target3))
target4_test <- subset(target4_test, select = -c(target2, target1, target3))
str(target4_train)
str(target4_test)

fit_tg4 <- lm(target4 ~ tag1+tag12+tag20+tag22+tag23+tag24+tag43
              +tag55+tag56+tag58+tag60+tag63+tag75, target4_train, na.action = na.omit)
summary(fit_tg4)
# Adjusted R-squared:  0.1199 


target4_test$predict <- predict(fit_tg4, target4_test)

mse(target4_test$target4, target4_test$predict)
mae(target3_test$target4, target4_test$predict)

# > target4_test$predict <- predict(fit_tg4, target4_test)
# > mse(target4_test$target4, target4_test$predict)
# [1] 0.1290802
# > mae(target3_test$target4, target4_test$predict)
# [1] 0







# dont work  !!!!!! 
for (i in colnames(train_data)) {
  for (j in colnames(train_data)) {
    if (i == 'target1' | i == 'target2') {
      message("target here")
    }
    else if (i != j) {
      corel <- cor.test(train_data[,i], train_data[,j])$estimate
      if (corel>0.5 & corel!=1) {
        # train_data <- train_data[!train_data[,i]]
        train_data <- train_data[,!colnames(train_data) %in% i]
      }
    }
  }
}


# don't works as a code above
i <- which(abs(cor(train_data)) > 0.5, arr.ind = T) # находим индексы
i <- i[i[,1] != i[,2],] # коряво, но работает - не учитываем параметры, которые коррелируют сами с собой

sprintf("%s ~ %s", names(train_data)[i[,1]], names(train_data)[i[,2]])

train_data.cleaned <- train_data[,!names(train_data) %in% rownames(i)] # убираем ВСЕ переменные, замешанные в коррелировании
train_data.cleaned <- train_data[,!names(train_data) %in% rownames(i)[1:nrow(i)/2]] # убираем только половину переменных, замешанных в коррелировании


#lets delete NA values



# ad pred columns
train_data$pr_tar1 <- 0
train_data$pr_tar2 <- 0
train_data$pr_tar3 <- 0
train_data$pr_tar4 <- 0


# split targets
target1_train <- subset(train_data, select = -c(target2, target3, target4, pr_tar4, pr_tar3, pr_tar2))
target2_train <- subset(train_data, select = -c(target1, target3, target4, pr_tar4, pr_tar3, pr_tar1))
target3_train <- subset(train_data, select = -c(target1, target2, target4, pr_tar4, pr_tar1, pr_tar2))
target4_train <- subset(train_data, select = -c(target1, target2, target3, pr_tar1, pr_tar3, pr_tar2))



set.seed(145)
sample = sample.split(train_data, SplitRatio = 0.70)
target1_train =subset(target1_train,sample ==TRUE)
target1_test =subset(target1_train,sample ==FALSE)
target2_train =subset(target2_train,sample ==TRUE)
target2_test =subset(target2_train,sample ==FALSE)
target3_train =subset(target3_train,sample ==TRUE)
target3_test =subset(target3_train,sample ==FALSE)
target4_train =subset(target4_train,sample ==TRUE)
target4_test =subset(target4_train,sample ==FALSE)

#linear regression for target1
fit <- lm(target1 ~ ., target1_train)
summary(fit)

target1_test$pr_tar1 <- predict(fit, target1_test)
mse(target1_test$target, target1_test$pr_tar1)



# separate to train and test


# linear model lets go

fit_tg1 <- lm(target1 ~ ., train_data_train, na.action=na.omit)
summary(fit_tg1)
# didnt work lets del col with less then 50% of values

cor_data <- cor(train_data_train, use = "pairwise.complete.obs")


#lets find and delete cor variables
which(cor_data>0.9&cor_data!=1, arr.ind = TRUE)


train_drop_set <- subset(train_data_train, select = -c(which(cor_data>0.9&cor_data!=1, arr.ind = TRUE)))



# del targets for target1
train_data_target1 <- subset(train_data_train, select = -c(target2, target3, target4))
test_data_target1 <- subset(train_data_test, select = -c(target2, target3, target4))
# deltagets fot target2
train_data_target2 <- subset(train_data_train, select = -c(target2, target3, target4))
test_data_target2 <- subset(train_data_test, select = -c(target2, target3, target4))






lm(target1 ~ ., train_drop_set, na.action = na.omit)



#return quanity of na values in train_data
sapply(train_data, function(x) sum(is.na(x)))

#return quanity of na values in test_data
sapply(test_data, function(x) sum(is.na(x)))

# lets see on data with missing values
# collect  % of data missing values


test_missing_proc <- as.data.frame(sapply(test_data, function(x) sum(is.na(x))/2000))
train_missin_proc <- as.data.frame(sapply(train_data, function(x) sum(is.na(x))/2000))





# create coll for predict
train_data$target1predict <- 0
train_data$target2predict <- 0
train_data$target3predict <- 0
train_data$target4predict <- 0

describe(train_data$tag1) 

is.na()
describe(exp(train_data$tag1))
qqplot(train_data$tag1, train_data$target3)
# linear regression not very usefool i think looks like sigmoid fuction better fit



# fill NA by mean
train_data_train[is.na(train_data_train)] <- mean(train_data_train)




#replace outliers with 3 sigma values





train_data_train <- as.data.frame(train_data_train)
# check outliers are you ther?
boxplot(train_data_train$tag1)
hist(train_data_train$tag1)








# а есть ли корреляция у предсказываемых значений
cor.test(train_data$target1, train_data$target2)
# data:  train_data$target1 and train_data$target2
# t = 300880, df = 199990, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.9999989 0.9999989
# sample estimates:
#   cor 
# 0.9999989 


cor.test(train_data$target1, train_data$target3)
# data:  train_data$target1 and train_data$target3
# t = -1139.4, df = 199990, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.9314492 -0.9302792
# sample estimates:
#   cor 
# -0.9308666



hist(train_data$target3)
#данные немного вправо смещены, попробуем логарифмировать и посмотреть корреляцию
cor.test(train_data$target3, train_data$target4)
# data:  train_data$target3 and log(train_data$target4)
# t = -127.1, df = 199990, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.2774355 -0.2693252
# sample estimates:
#   cor 
# -0.2733852 

# Четвертая переменная не коррелирует сильно с остальными


#интересно, в одинаковых ли местах пропущенные значения, если да то заполним значениями из
# других столбиков просто, всего 9 в самом начале...
length(train_data[is.na(train_data$target4)])
# 9
length(train_data[is.na(train_data$target3)])
# 9
length(train_data[is.na(train_data$target2)])
# 9
length(train_data[is.na(train_data$target1)])
# 9


heatmap(train_data)
str(train_data)

# раскидаем дату по разным столбикам
as.Date(train_data$X)


year <- train_data$X

# 
# x = data.frame(train_data$X)
# 
# y<-data.frame(Year=substr(x[,1],1,4),
#               Month=substr(x[,1],6,7),
#               Day=substr(x[,1],9,10))


train_data$year <-data.frame(Year=substr(data.frame(train_data$X)[,1],1,4)) # можно без года обойтись
train_data$month <-data.frame(Month=substr(data.frame(train_data$X)[,1],6,7)) 
train_data$day <-data.frame(Day=substr(data.frame(train_data$X)[,1],9,10)) 
train_data$hour <-data.frame(Hour=substr(data.frame(train_data$X)[,1],12,13)) 
train_data$minute <-data.frame(substr(data.frame(train_data$X)[,1],15,16)) 

train_data <- subset(train_data, select = -c(year))

# Замутим сабсет без годиков

train_subset <- subset(train_data, select = -c(X, month, day, hour, minute))


# To use Heatmap ferst we need remove na values
# Create a cor heatmap


#lets try to remove NA and create a corr heatmap

no_na_train_data <- subset(train_data)
no_na_train_data %>% drop_na(no_na_train_data) #didnt work... Why?

# drop na really bad idea )))
train_data2 <- na.omit(no_na_train_data)

# create subset without date
no_data_train_subset <- subset(train_data, select = -c(X))
fit  <- lm(target1 ~ ., no_data_train_subset)
summary(fit)

#return quanity of na values
sapply(train_data, function(x) sum(is.na(x)))


# what we can do we have many missed values? so what?
# cor will help
str(no_data_train_subset)
cor.test(no_data_train_subset$tag1, no_data_train_subset$tag8, na.rm=T)$estimate

describe(no_data_train_subset, na.rm = T)

