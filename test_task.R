# test application for ChemTech AI 
setwd("C:/Users/mrsim/Google ����/R statistic/test_task")
library(ggplot2)
library(psych)
library(caTools)

train_data  = read.csv("train_data_200k.csv")
test_data = read.csv("test_data_100k.csv")


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
qqplot(train_data$tag1, train_data$target2)

# separate to train and test
set.seed(145)
sample = sample.split(train_data,SplitRatio = 0.75)
train_data_train =subset(train_data,sample ==TRUE)
train_data_test =subset(train_data,sample ==FALSE)


# fill NA by mean
train_data_train[is.na(train_data_train)] <- mean()




#replace outliers with 3 sigma values



train_data_train <- as.data.frame(train_data_train)
# check outliers are you ther?
boxplot(train_data_train$tag1)
hist(train_data_train$tag1)








# � ���� �� ���������� � ��������������� ��������
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
#������ ������� ������ �������, ��������� ��������������� � ���������� ����������
cor.test(train_data$target3, train_data$target4)
# data:  train_data$target3 and log(train_data$target4)
# t = -127.1, df = 199990, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.2774355 -0.2693252
# sample estimates:
#   cor 
# -0.2733852 

# ��������� ���������� �� ����������� ������ � ����������


#���������, � ���������� �� ������ ����������� ��������, ���� �� �� �������� ���������� ��
# ������ ��������� ������, ����� 9 � ����� ������...
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

# ��������� ���� �� ������ ���������
as.Date(train_data$X)


year <- train_data$X

# 
# x = data.frame(train_data$X)
# 
# y<-data.frame(Year=substr(x[,1],1,4),
#               Month=substr(x[,1],6,7),
#               Day=substr(x[,1],9,10))


train_data$year <-data.frame(Year=substr(data.frame(train_data$X)[,1],1,4)) # ����� ��� ���� ��������
train_data$month <-data.frame(Month=substr(data.frame(train_data$X)[,1],6,7)) 
train_data$day <-data.frame(Day=substr(data.frame(train_data$X)[,1],9,10)) 
train_data$hour <-data.frame(Hour=substr(data.frame(train_data$X)[,1],12,13)) 
train_data$minute <-data.frame(substr(data.frame(train_data$X)[,1],15,16)) 

train_data <- subset(train_data, select = -c(year))

# ������� ������ ��� �������

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
