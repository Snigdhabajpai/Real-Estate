library(tidymodels)
  library(visdat)
  library(tidyr)
  library(car)
  library(pROC)
  library(ggplot2)
  library(tidyr)
  library(ROCit)
  library(dplyr)
#first we will read the dataset
  housing_train = read.csv(r"(C:\Users\Bajpa\Downloads\housing_train.csv)",stringsAsFactors = FALSE)
housing_test=read.csv(r"(C:\Users\Bajpa\Downloads\housing_test.csv)",stringsAsFactors = FALSE)
dim(housing_train)
view(housing_train)
names(housing_train )
setdiff(names(housing_train),names(housing_test))
glimpse(housing_train)
##We'll fill test's price column with NAs.
housing_test$Price=NA
head(housing_test)
##it is easier to manage  if you combine train and test
#in the beginning and then separate them once you are done with data preparation
housing_train $data='train'
housing_test$data='test'
h_all=rbind(housing_train,housing_test)

library(dplyr)

glimpse(h_all)

head(h_all)

##Next we'll create dummy variables for remaining categorical variables first by creating 
##a new function CreateDummies and then using sapply for creating dummies
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    2
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

char_logical=sapply(h_all,is.character)
cat_cols=names(h_all)[char_logical]

cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','Price'))]
cat_cols

# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy variables
for(col in cat_cols){
  h_all=CreateDummies(h_all,col,50)
}

glimpse(h_all)

##we can go ahead and separate training and test data BUT first we check NA values
h_all=h_all[!((is.na(h_all$Price)) & h_all$data=='train'), ]

for(col in names(h_all)){
  if(sum(is.na(h_all[,col]))>0 & !(col %in% c("data","Price"))){
    h_all[is.na(h_all[,col]),col]=mean(h_all[h_all$data=='train',col],na.rm=T)
  }
}

##Lets separate our two data sets and remove the unnecessary columns
## that we added while combining them.
housing_train=h_all %>% filter(data=='train') %>% select(-data)
housing_test=h_all %>% filter(data=='test') %>% select(-data,-Price)

glimpse(housing_train)
glimpse(housing_test)
#now lets divide the train dataset in the ratio 75:25.
set.seed(123)
s=sample(1:nrow(housing_train),0.75*nrow(housing_train))
housing_train_75=housing_train[s,] 
housing_test_25=housing_train[-s,] 
##Lets build a model on training data by checking VIF values as we need to remove multicollinearity


#summary_fit = summary(fit)
#p_values <- summary_fit$coefficients[, "Pr(>|t|)"]
------------------------------------------------
  library(car)
  fit=lm(Price~.,data=housing_train_75)
sort(vif(fit),decreasing = T)[1:3]

vif(fit)
fit=lm(Price~.-CouncilArea_,data=housing_train_75)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-CouncilArea_-Postcode,data=housing_train_75)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-CouncilArea_-Postcode-Distance,data=housing_train_75)
sort(vif(fit),decreasing = T)[1:3]

rm(fit)
fit=lm(Price~.,data=housing_train_75)

fit=step(fit)

summary(fit)

#We need to remove objects with high p-values (p > 0.05)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena,data=housing_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth,data=housing_train_75)


summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood,data=housing_train_75)
summary(fit)

fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale ,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda,data=housing_train_75)

summary(fit)

fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP,data=housing_train_75)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love-SellerG_Douglas,data=housing_train_75)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love-SellerG_Douglas
       -SellerG_Williams-SellerG_Village-SellerG_Stockdale-SellerG_Hodges
       -SellerG_McGrath-SellerG_Noel-SellerG_Gary-SellerG_Jas-SellerG_Fletchers
       -SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Ray-SellerG_Buxton
       -SellerG_Barry-SellerG_hockingstuart-SellerG_Nelson-CouncilArea_Monash
       -CouncilArea_Manningham-CouncilArea_Stonnington-CouncilArea_Darebin,data=housing_train_75)
summary(fit)
vif(fit)
#After removing VIF values > 10 and p values >0.05.
# now we make a prediction on our test data 
#based on our Linear Regression model that we built
test.predictions=predict(fit,newdata=housing_test_25)
test.predictions=round(test.predictions,1)
class(test.predictions)
#test.predictions contains the predicted price values for corresponding observations based on the model LRF. ####Calculating the RMSE and Plotting the graph.
plot(housing_test_25$Price,test.predictions)

res = housing_test_25$Price - test.predictions #(real value - predicted value)

RMSE_test_25=sqrt(mean(res^2))
RMSE_test_25

212467/RMSE_test_25

d=data.frame(real=housing_test_25$Price,predicted=test.predictions)
ggplot(d,aes(x=real,y=predicted))+geom_point()

#Predicting Real Estate Prices for the final Test Dataset.
#As per the model we built,we can now predict Prices for the Test dataset as follows:
  final_pred_test=predict(fit,newdata =housing_test)
  
  final_pred_test=round(final_pred_test,1)
class(final_pred_test)

write.csv(final_pred_test, "Snigdha_bajpai_P1_part2.csv ",row.names=F)



