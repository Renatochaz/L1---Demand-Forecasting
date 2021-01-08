install.packages("vars")
library(vars)
### Predict TS vs Real ###


### 12 days prevision - 01/05/2019 ###
as.numeric(fcast_no_holdout$mean)
table_predict <- rename(data.frame(as.numeric(fcast_no_holdout$mean)), predicted = 1)
table_predict$dia <- df_alf_cres_2$dia[749:760]
table_predict$vendas_clean <- df_alf_cres_2$clean_count[749:760]
table_predict$vendas_real <- df_alf_cres_2$vendas[749:760]
table_predict$deseasonal_cnt <- df_alf_cres_2$deseasonal_cnt[749:760]
table_predict <- table_predict[c(2,4,3,5,1)]
table_predict$predicted_clean <- as.numeric(fcast_no_holdout_clean$mean)
table_predict <- table_predict[c(1,2,4,5,3,6)]
table_predict$erro_previsao <- table_predict$vendas_real - table_predict$predicted_clean
table_predict$accuracy <- 100 - (abs(table_predict$erro_previsao / table_predict$vendas_real *100))

rm(table_predict)


test1_lin_mod <- lm(vendas_real[2:10] ~ predicted_clean[2:10], data = table_predict)
summary(test1_lin_mod)
?lm


table_predict_2$dia <- df_alf_cres_2$dia[500:511]
table_predict_2 <- rename(data.frame(as.numeric(fcast_no_holdout_ma$mean)), predicted = 1)
table_predict_2$ma <- count_ma[500:511]
table_predict_2$vendas <- df_alf_cres_2$vendas[500:511]
table_predict_2 <- table_predict_2[c(2,4,3,1)]
table_predict_2$erro <- table_predict_2$vendas - table_predict_2$predicted
table_predict_2$precisao <- 100 - (abs(table_predict_2$erro / table_predict_2$vendas * 100))



# preparing df for predict

predict_df <- table_predict_2[c(1,2,4,6)]
transitory <- df_alf_cres_2[500:511,c(7,8,10,11,12,17:30)]
teste_df <- cbind(predict_df, transitory)
??bigtime
