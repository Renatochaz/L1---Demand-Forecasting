### Load librarys and file ###
library(readr)
library (tidyverse)
df1 <- read_csv("G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Dados/10 - 10 - 2019/alface crespa.csv") # <- change name
####################################################################
### Subset useful columns and rename then ###
df2 <- df1 %>% select(39:47)
df3 <- df2 %>%
rename (
dia = "31/08/2016",
hora = "X40",
operacao = "Saldo Anterior",
tipo = "X42",
quantidade = "-13,00", # <- Change column name
saldo = "X44",
custo_de_aquisicao = "X45",
custo_de_reposicao = "X46",
preco_de_venda = "X47"
)
### Change structure to char and int ###
df3 [] <- lapply (df3, as.character)
df4 <- df3
### IF quantidade = decimal, use as follow ###
df4$quantidade <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$quantidade)))
###IF not use as follow ###
df4$quantidade <- as.integer(df4$quantidade)

#######
### Subset only sales ###
df5 <- df4 %>% group_split(operacao, keep = TRUE)
df7 <- df5

#######

### Group by day instead of hour###
df8 <- df7 %>%
group_by(dia, operacao, tipo) %>%
mutate(quantidade = sum(quantidade)) %>%
select(-hora) %>%
ungroup %>%
distinct(dia, operacao, tipo, .keep_all = TRUE)
### convert to numeric /100 with quantidade ###
df9 <- df8
df9$quantidade <- as.numeric(df9$quantidade)/100
df9$custo_de_aquisicao <- as.numeric(gsub(",", ".", gsub("\\.", "", df9$custo_de_aquisicao)))
df9$custo_de_reposicao <- as.numeric(gsub(",", ".", gsub("\\.", "", df9$custo_de_reposicao)))
df9$preco_de_venda <- as.numeric(gsub(",", ".", gsub("\\.", "", df9$preco_de_venda)))

# <- Check comma separators and use appropriate code depending on numeric input###
#In decimals use /100 on custos e preco
df9$custo_de_aquisicao <- df9$custo_de_aquisicao/100
df9$custo_de_reposicao <- df9$custo_de_reposicao/100
df9$preco_de_venda <- df9$preco_de_venda/100
### Subset useless columns ###
df10 <- df9
df10 <- subset(df10, select = -c(2,3, 5))
### Check NA and replace with median ###
sum(is.na(df10$dia))
sum(is.na(df10$operacao))
sum(is.na(df10$quantidade))
sum(is.na(df10$custo_de_aquisicao))
sum(is.na(df10$custo_de_reposicao))
sum(is.na(df10$preco_de_venda))
df11 <- df10
df11$custo_de_reposicao[is.na(df11$custo_de_reposicao)] <- median(df11$custo_de_reposicao, na.rm=TRUE)
sum(is.na(df11$custo_de_reposicao))
### Write csv ###
write.csv(df12, file="G:/Meu Drive/Tactin/Trabalhos - Análise de Dados/Serv bem/R-project/R/Dados/10 - 10 - 2019/Clean/clean_alface_crespa.csv", row.names=FALSE) # <- change file name

### END CODE ###
