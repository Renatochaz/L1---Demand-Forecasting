##### Quantile regression #####
library(quantreg)
library(AER)
library(lmtest)
library(car)
# Summary to check if there is NA in variables
summary(`alface crespa`) # <- no NA's detected in important variables

# Test of Hyphotesis 
# Fitting a simple multiple regression model
multiple_model <- lm(vendas ~ preco_de_venda + dum_chuva + temp_media + dum_feriado + dum_ferias_escolares + fev + mar + abr + mai + jun + jul + ago + set + out + nov + dez + before_fer + after_fer + promocao, data = `alface crespa`)
# Checking linearity of continuous variables
avPlot(multiple_model,"preco_de_venda")
avPlot(multiple_model, "temp_media")
# Linearity OK

# Testing normality
shapiro.test(multiple_model$residuals) 
# Residuals are NOT normal, cant use simple multiple regression

# Testing homoscedasticity

bptest(multiple_model)

# Reject null hypothesis, there is heteroskedasciticy

# Durbin Watson test
dwtest(multiple_model)

# There is autocorrelation in this model

# Checking multicoliearity

vif(multiple_model)

# No problems of multicolinearity since there is no value greater than 10


# Final summary:
# linearity OK
# Multicolinearity OK
# Normal NO
# Homoskedacity NO
# Autocorrelation NO

# We can use quantile regression but the results are questionable due the problem of autocorrelation

# Qr fitting
to_qr <- vendas ~ preco_de_venda + dum_chuva + temp_media + dum_feriado + dum_ferias_escolares + fev + mar + abr + mai + jun + jul + ago + set + out + nov + dez + before_fer + after_fer + promocao
qr_model <- rq(to_qr, data = `alface crespa`)
# Checking results at 0.5
summary(qr_model, se = "ker")

# At 0.5 quantile , chuva, ferias escolares,fev,mar,abr,maio, antes e depois feriado não são significativos, promoção é significativo a 10%

# Analyzing all quartiles

full_qr <- rq(to_qr, tau = seq(0.25,0.75, by =0.25), data = `alface crespa`)
full_qrs <- summary(full_qr, se = "ker")
full_qrs

# Analisando os 3 quartils, temos as seguintes covariáveis aparecendo como significativas:
# preco de venda, temperatura media, dummy de feriado, maio a dezembro e promocao em diferentes quartils
