#AV BLACKFriday competetion using Multiple linear regression analysis using matrices

library('stringr') # string manipulation
library('dplyr') # data manipulation


data <- read.csv(unz("train-corrected.zip", "train.csv"), header=T, quote="\"", sep=",")
test = read.csv('test_Comb.csv')
test=test[,2:12]
# check the column data values

# Null value check
names(test[1,3:11])

data$Product_Category_2[is.na(data$Product_Category_2)] <- 0
data$Product_Category_3[is.na(data$Product_Category_3)] <- 0

test$Product_Category_2[is.na(test$Product_Category_2)] <- 0
test$Product_Category_3[is.na(test$Product_Category_3)] <- 0


attach(data)
fit = lm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, 
         contrasts.arg = list(
           User_ID = contr.treatment(n = 5891, contrasts = FALSE),
           Product_ID = contr.treatment(n = 3631, contrasts = FALSE),
           Gender = contr.treatment(n = 2, contrasts = FALSE),
           Age = contr.treatment(n = 7, contrasts = FALSE),
           Occupation = contr.treatment(n = 21, contrasts = FALSE),
           City_Category = contr.treatment(n = 3, contrasts = FALSE),
           Stay_In_Current_City_Years = contr.treatment(n = 5, contrasts = FALSE),
           Marital_Status = contr.treatment(n = 2, contrasts = FALSE),
           Product_Category_1 = contr.treatment(n = 20, contrasts = FALSE),
           Product_Category_2 = contr.treatment(n = 18, contrasts = FALSE),
           Product_Category_3 = contr.treatment(n = 16, contrasts = FALSE)
         ), na.action=na.omit)


summary(fit)




pvalue = predict(fit,data.frame(test[]))

test$Purchase = pvalue
write.csv(test[,c(1,2,12)], 'submit_data.csv',row.names = FALSE)
