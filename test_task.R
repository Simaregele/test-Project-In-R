# test application for ChemTech AI 
setwd("C:/Users/mrsim/Google ƒиск/R statistic/test_task")
library(ggplot2)
library(psych)



train_data  = read.csv("train_data_200k.csv")
test_data = read.csv("test_data_100k.csv")



# separate to train and test
set.seed(145)
sample = sample.split(train_data,SplitRatio = 0.75)
train_data_train =subset(train_data,sample ==TRUE)
train_data_test =subset(train_data,sample ==FALSE)

# linear model lets go
# del date 
train_data_train <- subset(train_data_train, select = -c(target2, target3, target4))
train_data_test <- subset(train_data_test, select = -c(target2, target3, target4))


fit_tg1 <- lm(target1 ~ ., train_data_train, na.action=na.omit)
summary(fit_tg1)
# didnt work lets del col with less then 50% of values

cor_data <- cor(train_data_train, use = "pairwise.complete.obs")


#lets find and delete cor variables
length(which(cor_data>0.9&cor_data!=1, arr.ind = TRUE))










#return quanity of na values in train_data
sapply(train_data, function(x) sum(is.na(x)))

#return quanity of na values in test_data
sapply(test_data, function(x) sum(is.na(x)))

# lets see on data with missing values
# collect  % of data missing values


test_missing_proc <- as.data.frame(sapply(test_data, function(x) sum(is.na(x))/2000))
train_missin_proc <- as.data.frame(sapply(train_data, function(x) sum(is.na(x))/2000))


# delete coll that have less 75% of values
train_data <- train_data[,sapply(train_data, function(x) (sum(is.na(x))/2000))<0.75]



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








# а есть ли коррел€ци€ у предсказываемых значений
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
#данные немного вправо смещены, попробуем логарифмировать и посмотреть коррел€цию
cor.test(train_data$target3, train_data$target4)
# data:  train_data$target3 and log(train_data$target4)
# t = -127.1, df = 199990, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.2774355 -0.2693252
# sample estimates:
#   cor 
# -0.2733852 

# „етверта€ переменна€ не коррелирует сильно с остальными


#интересно, в одинаковых ли местах пропущенные значени€, если да то заполним значени€ми из
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

# «амутим сабсет без годиков

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

