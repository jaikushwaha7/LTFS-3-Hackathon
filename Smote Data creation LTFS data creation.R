(1+perc.over) *7040 / ((1+perc.over)*7040 + 25151) 
setwd("D:/Study/Hackathon analytics vidhya/Hackathon LTFS 3 30012021")

df_test <- read.csv('df_test_Data2.csv', header = T)
df_train <- read.csv('df_train_Data2.csv', header = T)

library(dplyr)

df_train %>% anyNA()
df_test %>% anyNA()

str(df_train)

df_train <- mutate_if(df_train, is.character, as.factor)
df_test <- mutate_if(df_test, is.character, as.factor)

summary(df_train)

df_train%>%
  purrr::map_df(~ sum(is.na(.)))

sum(is.na(df_train))

na_col_pct<- summarise_all(df_train, ~(sum(is.na(.)*100 / nrow(df_train))))
na_col_pct
na_col<-summarise_all(df_train, ~(sum(is.na(.)))) 

na_col %>%
  select_if(na_col>0)

list_na <- colnames(df_train)[ apply(df_train, 2, anyNA) ]
list_na


df_train %>%
  select(list_na) %>%
  summary()

# Median of Missing 
median_missing <- apply(df_train[,colnames(df_train) %in% list_na], 2,median, na.rm =  TRUE)
median_missing
df_train_impute_median <- df_train %>%
  mutate(Loan_Age  = ifelse(is.na(Loan_Age), median_missing[1], Loan_Age), 
         paymentmode_cd_cnt  = ifelse(is.na(paymentmode_cd_cnt ), median_missing[2], paymentmode_cd_cnt ),
         area_cd_cnt  = ifelse(is.na(area_cd_cnt ), median_missing[3], area_cd_cnt ),
         city_cd_cnt  = ifelse(is.na(city_cd_cnt ), median_missing[4], city_cd_cnt ),
         state_cd_cnt  = ifelse(is.na(state_cd_cnt ), median_missing[5], city_cd_cnt ))


sapply(df_train_impute_median, function(x) sum(is.na(x)))

#str(MSME_Data_Modified_v1.5_clean_without_NA)
write.csv(df_train_impute_median, "df_train_imputed_median.csv", row.names = F)
write.csv(df_test, "df_test_imputed_median.csv", row.names = F)

library(DMwR)
library(VIM)

## KNN Imputation
#df_train_impute_knn <- kNN(df_train, k = 5) # perform knn imputation.
#anyNA(data_msme_impute_knn)
#Check for missing
sapply(data_msme_impute_knn, function(x) sum(is.na(x)))

write.csv(df_train_impute_median, "df_train_imputed_median.csv", row.names = F)
write.csv(df_test, "df_test_imputed_median.csv", row.names = F)



df_smote<- df_train_impute_median
df_smote$Top.up.Month<- as.factor(df_smote$Top.up.Month)
table(df_smote$Top.up.Month)
df_smote<- SMOTE(Top.up.Month ~ .,df_smote)
df_smote<- SMOTE(Top.up.Month ~ .,df_smote, perc.over = 600, perc.under = 100)
table(df_smote$Top.up.Month)

First_Imbalence_recover <- DMwR::SMOTE(Top.up.Month ~ ., df_smote, perc.over = 2000,perc.under=100)
table(First_Imbalence_recover$Top.up.Month)
Second_Imbalence_recover <- DMwR::SMOTE(Top.up.Month ~ ., First_Imbalence_recover, perc.over = 2000,perc.under=100)
table(Second_Imbalence_recover$Top.up.Month)
Final_Imbalence_recover <- DMwR::SMOTE(Top.up.Month ~ ., Second_Imbalence_recover, perc.over = 2000,perc.under=200)
table(Final_Imbalence_recover$Top.up.Month)


write.csv(df_smote, 'MSME data featEngg_Smote.csv', row.names = F)
smoted_data <- SMOTE(loan_default~., df, perc.over=100)

######################################################################################

df <- read.csv('MSME data_FeatEngg_CorrFeatRemoved.csv')
df_featengg <- read.csv('MSME Data Modified_v1.9_featureEngineered.csv')
df$loan_default<- df_featengg$loan_default
table(df$loan_default)

library(DMwR)

df_smote<- df
df_smote$loan_default<- as.factor(df_smote$loan_default)
table(df_smote$loan_default)
df_smote<- SMOTE(loan_default ~ .,df_smote, perc.over = 100, perc.under = 200)

table(df_smote$loan_default)

write.csv(df_smote, 'MSME data featEngg_corrRemoved_Smote.csv', row.names = F)
smoted_data <- SMOTE(loan_default~., df, perc.over=100)

#######################################################################################

df <- read.csv('MSME Data Modified_v1.10_clean_withoutNA_ImpFeatures10.csv')
df_featengg <- read.csv('MSME Data Modified_v1.9_featureEngineered.csv')
df$loan_default<- df_featengg$loan_default
table(df$loan_default)

library(DMwR)

df_smote<- df
df_smote$loan_default<- as.factor(df_smote$loan_default)
table(df_smote$loan_default)
df_smote<- SMOTE(loan_default ~ .,df_smote, perc.over = 100, perc.under = 200)

table(df_smote$loan_default)

write.csv(df_smote, 'MSME data featEngg_top10_Smote.csv', row.names = F)
smoted_data <- SMOTE(loan_default~., df, perc.over=100)
