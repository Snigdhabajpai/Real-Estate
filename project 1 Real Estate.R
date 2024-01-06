
housing_train = read.csv(r"(C:\Users\Bajpa\Downloads\housing_train.csv)",stringsAsFactors = FALSE)
housing_test=read.csv(r"(C:\Users\Bajpa\Downloads\housing_test.csv)",stringsAsFactors = FALSE)
head(housing_train )
#part 1 quiz
#1
var(housing_train$Price)
#2
sum(is.na(housing_train$YearBuilt))
#3
housing_train$Type
max__av <- housing_train %>%
  group_by(Type) %>%
  summarise(type_house = mean(Price, na.rm = TRUE)) 
1294320-901936
View(housing_train)
library(dplyr)
#4
unique(housing_train$Postcode)
#5
is.character(housing_train$Postcode)
is.numeric(housing_train$Postcode)
#6
library(ggplot2)
ggplot(housing_train,aes(x=Distance))+geom_histogram()
#7
View(housing_train)
max_price <- housing_train %>%
  group_by(Suburb) %>%
  summarise(sum_price = sum(Price, na.rm = TRUE)) %>%
  arrange(desc(sum_price))

max_selling_seller <- max_price$Suburb

#ques8
max_avg_price <- housing_train %>%
  group_by(CouncilArea) %>%
  summarise(mean_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))
max_avg_seller= max_avg_price$CouncilArea

#ques9
max__var <- housing_train %>%
  group_by(CouncilArea) %>%
  summarise(var_price = var(Price, na.rm = TRUE)) %>%
  arrange(desc(var_price))
max_var_price= max__var$CouncilArea



