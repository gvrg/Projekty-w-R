library(foreign)
library(dplyr)
library(stringr)

rm(list = ls())


diagnoza = read.dta('D:\\Analityka\\Dane\\pgss9297.dta')


dim(diagnoza)

diagnoza %>%
    group_by(pgssyear) %>%
    count()

diag97 <- diagnoza %>%
    filter(pgssyear == "1997/98r", as.integer(life) < 4) %>%
    select(life, region8, size, hompop, sex, age,
           siops, rincome, marital, childs, finalter, hapunhap, polview)


diag97 %>%
    group_by(region8) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)

diag97 %>%
    group_by(marital) %>%
    na.omit(marital) %>%
    filter(n() > 20) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)


diag97 %>%
    group_by(childs) %>%
    na.omit(childs) %>%
    filter(n() > 20) %>%
    summarise(pasja = mean(as.numeric(life))) %>%
    arrange(pasja)



## Przygotowanie danych

diag97['life_int'] <- as.integer(diag97$life)

diag97['life_int'] <- cut(diag97$life_int, breaks = c(0,1,3), labels = c(1,0))

diag97 %>% group_by(life_int) %>%
    count()

sapply(diag97, levels)


int = c("size", "hompop", 'sex')

change_integer <- function(x) {
    x <- as.integer(x)
}

diag97[int] <- sapply(diag97[int], change_integer)

diag97['marital_bin'] <- as.integer(diag97$marital)
diag97$marital_bin <- cut(diag97$marital_bin,
                          breaks = c(0,1,5),
                          labels = c(1,0))

diag97 <- diag97 %>% 
    filter(as.integer(finalter) < 4) %>%
    na.omit(marital_bin) 

head(diag97$finalter )
levels(diag97$finalter)
levels(diag97$finalter) <- c("pogarsza³a siê","pozostawa³a taka sama", "poprawia³a siê", "nie wiem","brak danych")
diag97$finalter <- as.integer(diag97$finalter)
 

# Modelowanie 

library(randomForest)
library(caret)

logisticReg <- train(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
                     data = diag97,
                     method = 'glm',
                     trControl = trainControl(method = 'repeatedcv',
                                              repeats = 5))

logisticReg

train_ind <- sample(seq_len(nrow(diag97)), size = smp_size)

train <- diag97[train_ind, ]
test  <- diag97[-train_ind, ]
                             
model  <- glm(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
              data = train, family = 'binomial')
                             
summary(model)

logit_pred <- predict(model, test)
head(logit_pred)
summary(logit_pred)
test$logitPred <- ifelse(logit_pred > 0.5, 1,0)

confusionMatrix(data = test$logitPred,
                reference = test$life_int,
                positive = '1')


## Klasyfikacja
smp_size <- floor(0.75* nrow(diag97))

set.seed(123)





RFmodel <- randomForest(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
                        data = train, ntree = 5)

RFTest <- predict(RFmodel, test, type = 'prob')

head(RFTest)

test$prob <- RFTest[, '1']
test$class <- predict(RFmodel, test)


sensitivity(data = test$class,
            reference = test$life_int,
            positive = '1')

specificity(data = test$class,
            reference = test$life_int,
            negative = '0')

confusionMatrix(data = test$class,
            reference = test$life_int,
            positive = '1')

library(rpart)
library(rpart.plot)

Rpart_model <- rpart(life_int ~ sex + age + hompop + size + marital_bin + siops + finalter,
                     data = train, method = 'class')
Rpart_Test <- predict(RFmodel, test, type = 'prob')

head(Rpart_Test)

pred <- as.data.frame(pred)
head(pred)
test$pred <- ifelse(pred$`1` > 0.5, 1,0)
test$pred <- as.factor(test$pred)
class(test$life_int)
levels(test$pred)


confusionMatrix(data = test$pred,
                reference = test$life_int,
                positive = '1')

sensitivity(data = test$pred,
                reference = test$life_int,
                positive = '1')

specificity(data = test$pred,
                reference = test$life_int,
                negative =  '0')

rpart.plot(Rpart_model)
