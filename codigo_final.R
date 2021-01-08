### Load librarys and file ###
library(readr)
library (tidyverse)
library (lubridate)


nomes <- c("alface crespa","alface lisa","alho solto kg","batata comum granel kg","batata deoce roxa kg","brocoli ninja in","brocolis un","cebola granel kg","cenoura kg","cheiro verde maco","tomate kg")
### Read all DBS and execute clean db
for (k in 1:11){
db_transitoria <- read_csv(paste0('G:/Meu Drive/Tactin/',
                                  'Trabalhos - Análise de Dados/Serv bem/',
                                  'R-project/R/Dados/10 - 10 - 2019/',
                                  nomes[k],".csv"))

################################################
################### Clean db ###################
################################################

### Subset useful columns and rename then ###
df1 <- db_transitoria
df2 <- df1 %>% select(39:47)
df3 <- df2 %>%
  rename (
    dia = 1,
    hora = 2,
    operacao = 3,
    tipo = 4,
    quantidade = 5,
    saldo = 6,
    custo_de_aquisicao = 7,
    custo_de_reposicao = 8,
    preco_de_venda = 9
  )

df4 <- df3
# Change structura to numeric and apply correction to dots(commas)
df4[1:8] <- lapply(df4[1:8], as.character)
df4$quantidade <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$quantidade)))
df4$custo_de_aquisicao <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$custo_de_aquisicao)))
df4$custo_de_reposicao <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$custo_de_reposicao)))
df4$preco_de_venda <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$preco_de_venda)))

# Group by day
df5 <- df4 %>%
  group_by(dia, operacao, tipo) %>%
  mutate(quantidade = sum(quantidade)) %>%
  select(-hora) %>%
  ungroup %>%
  distinct(dia, operacao, tipo, .keep_all = TRUE)

### Creatind DB with columns of vendas, entradas and quebras###

# Subsetting DB variables
vendas <- df5 %>%
  filter(operacao == "Venda PDV")
quebras <- df5 %>%
  filter(operacao == "Quebras")
entradas <- df5 %>%
  filter(operacao == "Entrada NF")

df6 <- df5


# Matching vendas and reordering
df6$vendas <- 0
i <- 1
j<- 1
for (i in 1:nrow(vendas)) {
  for (j in 1:nrow(df6)) {
  ifelse(vendas$dia[i] == df6$dia[j] & df6$operacao[j] == "Venda PDV" ,(df6$vendas[j] <- vendas$quantidade[i]),"")
  }
}
# check
sum(df6$vendas > 0) == nrow(vendas)
df6 <- df6[c(1,2,9,4,3,5,6,7,8)]
i <- 1
j<- 1
df7<- df6

# Matching quebras and reordering
df7$quebras <- 0
for (i in 1:nrow(quebras)) {
  for (j in 1:nrow(df7)) {
    ifelse(quebras$dia[i] == df7$dia[j] & df7$operacao[j] == "Quebras" ,(df7$quebras[j] <- quebras$quantidade[i]),"")
  }
}
#check
sum(df7$quebras > 0) == nrow(quebras)
df7 <- df7[c(1,2,3,10,4,5,6,7,8,9)]
i <- 1
j<- 1
df8 <- df7

# Matching entradas and reordering
df8$entradas <- 0
for (i in 1:nrow(entradas)) {
  for (j in 1:nrow(df8)) {
    ifelse(entradas$dia[i] == df8$dia[j] & df8$operacao[j] == "Entrada NF" ,(df8$entradas[j] <- entradas$quantidade[i]),"")
  }
}
#check
sum(df8$entradas > 0) == nrow(entradas)
df8 <- df8[c(1,2,3,4,11,5,6,7,8,9,10)]
df9 <- df8
rm(i,j)

#Removing useless operations observation and columns
df9 <- df8%>% 
  filter (operacao == "Venda PDV" | operacao == "Entrada NF" | operacao == "Quebras")
df10 <- df9 %>%
  select(1,3:6,9:11)
df10 <- df10[-c(5)]


assign(nomes[k],df10)
rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,entradas,vendas,quebras)
}


################################################
################### END FORL ###################
################################################

### Ajustando escalas

## escalas a serem ajustadas

#(quantidade,custo reposicao) alface crespa, lisa,
# custos: alho solto, batata e batata,cebola, tomate
# tudo: brocoli ninja, brocolis
#tudo ok: cenoura
#quantidade: cheiro verde


#Adjusting scales
`alface crespa`[c(2:4,6)] <- `alface crespa`[c(2:4,6)]/100
`alface lisa`[c(2:4,6)] <- `alface lisa`[c(2:4,6)]/100
`alho solto kg`[c(5:7)] <- `alho solto kg`[c(5:7)]/100
`batata comum granel kg`[c(5:7)] <- `batata comum granel kg`[c(5:7)]/100
`batata deoce roxa kg`[c(5:7)] <- `batata deoce roxa kg`[c(5:7)]/100
`brocoli ninja in`[c(2:7)] <- `brocoli ninja in`[c(2:7)]/100
`brocolis un`[c(2:7)] <- `brocolis un`[c(2:7)]/100
`cebola granel kg`[c(5:7)] <- `cebola granel kg`[c(5:7)]/100
`cheiro verde maco`[c(2:4)] <- `cheiro verde maco`[c(2:4)]/100
`tomate kg`[c(5:7)] <- `tomate kg`[c(5:7)]/100
rm(db_transitoria)

### Grouping days
for (d in 1:length(nomes)) {
  df_transitoria_2 <- get(nomes[d])
  df_transitoria_2 <- df_transitoria_2 %>%
    group_by(dia) %>%
    summarise_all(list(max))
  assign(nomes[d],df_transitoria_2)
  rm(df_transitoria_2)
}

########## END DB CLEAN ##########

### To virgulas for Richard
# Change structura to numeric and apply correction to dots(commas)

for (i in 1:length(nomes)){
  
df1 <- get(nomes[i])
df1$vendas <- gsub("\\.", ",",df1$vendas)
df1$entradas <- gsub("\\.", ",",df1$entradas)
df1$quebras <- gsub("\\.", ",",df1$quebras)
df1$custo_de_aquisicao <- gsub("\\.", ",",df1$custo_de_aquisicao)
df1$custo_de_reposicao <- gsub("\\.", ",",df1$custo_de_reposicao)
df1$preco_de_venda <- gsub("\\.", ",",df1$preco_de_venda)

assign(paste0("virgula_",nomes[i]),df1)
write.csv(df1,paste0('G:/Meu Drive/Tactin/',
                               'Trabalhos - Análise de Dados/Serv bem/',
                               'R-project/R/Dados/10 - 10 - 2019/',
                               'Clean_final/[CLEAN_VIRGULA]',nomes[i],".csv"), row.names = FALSE)
rm(df1)
}

### Saving clean DB

for (i in 1:length(nomes)){
  write.csv(get(nomes[i]),paste0('G:/Meu Drive/Tactin/',
                                   'Trabalhos - Análise de Dados/Serv bem/',
                                   'R-project/R/Dados/10 - 10 - 2019/',
                                   'Clean_final/[CLEAN]',nomes[i],".csv"), row.names = FALSE)
}



################################################
################## DB NEW INFO #################
################################################

########### Weather information ###############

### Loading data
dados_precipitacao <- read_delim(paste0('G:/Meu Drive/Tactin/',
                                        'Trabalhos - Análise de Dados/Serv bem/',
                                        'Dados_10_10 - Backup (Não mexer)/',
                                        "Dados_precipitacao.csv"),
                                 ";", escape_double = FALSE,
                                 col_types = cols(Estacao = col_skip()),
                                 trim_ws = TRUE)
### Preparing DFS

# Grouped by day df
dados_precipitacao[is.na(dados_precipitacao)] <- 0
chuva_by_day <- dados_precipitacao %>%
  group_by(Data) %>%
  summarise_all(list(max))

# Medium temp df
dftemp <- chuva_by_day
dftemp <- dftemp %>%
  subset(TempMaxima != 0)
dftemp$tempMedia <- ifelse(dftemp$TempMinima == 0,(dftemp$TempMaxima+10)/2,(dftemp$TempMaxima + dftemp$TempMinima)/2)

### Applying info to all
for (i in 1:length(nomes)) {
  
df1 <- get(nomes[i])

# Initializing variables
df1$dum_chuva <- 0
df1$precipitacao <- 0
df1$temp_media <- 0

# Dummy chuva
for (c in 1:nrow(chuva_by_day)) {
  df1$dum_chuva[which(df1$dia == chuva_by_day$Data[c] & chuva_by_day$Precipitacao[c] > 0)] <- 1
}

# Precipitacao
for (c in 1:nrow(chuva_by_day)) {
  df1$precipitacao[which(df1$dia == chuva_by_day$Data[c] & chuva_by_day$Precipitacao[c] > 0)] <- chuva_by_day$Precipitacao[c]
}

# Temp media
for (c in 1:nrow(dftemp)) {
  df1$temp_media[which(df1$dia == dftemp$Data[c])] <- dftemp$tempMedia[c]
}

# Drop lack of data
df1 <- df1 %>%
  subset(temp_media != 0)

assign (nomes[i],df1)

rm(df1)

}


########### Holiday information ###############

# Vector holidays
dias_feriado <- c("07/09/2016","12/10/2016","15/10/2016","28/10/2016","02/11/2016","04/11/2016","15/11/2016","25/12/2016","01/01/2017","27/02/2017","28/02/2017","01/03/2017","14/04/2017","21/04/2017","01/05/2017","15/06/2017","09/07/2017","15/08/2017","07/09/2017","12/10/2017","15/10/2017","28/10/2017","02/11/2017","04/11/2017","15/11/2017","25/12/2017","01/01/2018","12/02/2018","13/02/2018","14/02/2018","30/03/2018","21/04/2018","01/05/2018","31/05/2018","09/07/2018","15/08/2018","07/09/2018","12/10/2018","15/10/2018","28/10/2018","02/11/2018","04/11/2018","15/11/2018","25/12/2018","01/01/2019","04/03/2019","05/03/2019","06/03/2019","19/04/2019","21/04/2019","01/05/2019","20/06/2019","09/07/2019","15/08/2019","07/09/2019")

# Vector férias
f2 <- strftime(seq(as.Date("2016/12/20"), as.Date("2017/02/02"), by="days"), "%d/%m/%Y")
f3 <- strftime(seq(as.Date("2017/06/28"), as.Date("2017/07/31"), by="days"), "%d/%m/%Y")
f4 <- strftime(seq(as.Date("2017/12/21"), as.Date("2018/02/01"), by="days"), "%d/%m/%Y")
f5 <- strftime(seq(as.Date("2018/06/27"), as.Date("2018/08/01"), by="days"), "%d/%m/%Y")
f6 <- strftime(seq(as.Date("2018/12/20"), as.Date("2019/02/01"), by="days"), "%d/%m/%Y")
f7 <- strftime(seq(as.Date("2019/06/27"), as.Date("2019/07/31"), by="days"), "%d/%m/%Y")
dias_ferias <- c(f2,f3,f4,f5,f6,f7)

# Applying info to all

for (i in 1:length(nomes)) {
  
  df1 <- get(nomes[i])
  
  # Initializing variables
  df1$dum_feriado <- 0
  df1$dum_ferias_escolares <- 0

  
  # Feriados
  for (f in 1:length(dias_feriado)) {
    df1$dum_feriado[which(df1$dia == dias_feriado[f])] <- 1
  }
  
  # Ferias escolares
  for (f in 1:length(dias_ferias)) {
    df1$dum_ferias_escolares[which(df1$dia == dias_ferias[f])] <- 1
  }
  
  # Ending
  assign (nomes[i],df1)
  rm(df1)
}

########### Weekday , Month and Season information ###############

# Setting seasons
inv_2016 <- strftime(seq(as.Date("2016/06/21"), as.Date("2016/09/21"), by="days"), "%d/%m/%Y")
prim_2016 <- strftime(seq(as.Date("2016/09/22"), as.Date("2016/12/20"), by="days"), "%d/%m/%Y")
ver_2016_2017 <- strftime(seq(as.Date("2016/12/21"), as.Date("2017/03/19"), by="days"), "%d/%m/%Y")
outon_2017 <- strftime(seq(as.Date("2017/03/20"), as.Date("2017/06/20"), by="days"), "%d/%m/%Y")
inv_2017 <- strftime(seq(as.Date("2017/06/21"), as.Date("2017/09/21"), by="days"), "%d/%m/%Y")
prim_2017 <- strftime(seq(as.Date("2017/09/22"), as.Date("2017/12/20"), by="days"), "%d/%m/%Y")
ver_2017_2018 <- strftime(seq(as.Date("2017/12/21"), as.Date("2018/03/19"), by="days"), "%d/%m/%Y")
outon_2018 <- strftime(seq(as.Date("2018/03/20"), as.Date("2018/06/20"), by="days"), "%d/%m/%Y")
inv_2018 <- strftime(seq(as.Date("2018/06/21"), as.Date("2018/09/21"), by="days"), "%d/%m/%Y")
prim_2018 <- strftime(seq(as.Date("2018/09/22"), as.Date("2018/12/20"), by="days"), "%d/%m/%Y")
ver_2018_2019 <- strftime(seq(as.Date("2018/12/21"), as.Date("2019/03/19"), by="days"), "%d/%m/%Y")
outon_2019 <- strftime(seq(as.Date("2019/03/20"), as.Date("2019/06/20"), by="days"), "%d/%m/%Y")
inv_2019 <- strftime(seq(as.Date("2019/06/21"), as.Date("2019/09/22"), by="days"), "%d/%m/%Y")
prim_2019 <- strftime(seq(as.Date("2019/09/23"), as.Date("2019/12/21"), by="days"), "%d/%m/%Y")

outono <- c(outon_2017,outon_2018,outon_2019)
inverno <- c(inv_2016,inv_2017,inv_2018,inv_2019)
primavera <- c(prim_2016,prim_2017,prim_2018,prim_2019)
verão <- c(ver_2016_2017,ver_2017_2018,ver_2018_2019)

# Applying to all
for (i in 1:length(nomes)){
  
  # Setting working db and variables
  df1 <- get(nomes[i])
  df1$estacao <- ""
  df1$dia_semana <- ""
  df1$mes <- ""
  df1$poxicct_dia <- strptime(as.character(df1$dia), "%d/%m/%Y")
  df1$fev <- 0
  df1$mar <- 0
  df1$abr <- 0
  df1$mai <- 0
  df1$jun <- 0
  df1$jul <- 0
  df1$ago <- 0
  df1$set <- 0
  df1$out <- 0
  df1$nov <- 0
  df1$dez <- 0


  # Seasons
  for (s in 1:nrow(df1)){
    if (length(which(df1$dia[s] == outono)) != 0){
      df1$estacao[s] <- "Outono"
    } else if (length(which(df1$dia[s] == inverno)) !=0){
      df1$estacao[s] <- "Inverno"
    } else if (length(which(df1$dia[s] == primavera)) != 0){
      df1$estacao[s] <- "Primavera"
    } else if (length(which(df1$dia[s] == verão)) != 0) {
      df1$estacao[s] <- "Verão"
    }
  }
  
  df1$estacao <- as.factor(df1$estacao)
  
  # Week days
  df1$dia_semana <- wday(df1$poxicct_dia, label = TRUE)
  
  # Months
  df1$mes <- month(df1$poxicct_dia, label = TRUE)
  
  # Months dummies fev - dez
  df1[which(df1$mes == "fev"),c("fev")] <- 1
  df1[which(df1$mes == "mar"),c("mar")] <- 1
  df1[which(df1$mes == "abr"),c("abr")] <- 1
  df1[which(df1$mes == "mai"),c("mai")] <- 1
  df1[which(df1$mes == "jun"),c("jun")] <- 1
  df1[which(df1$mes == "jul"),c("jul")] <- 1
  df1[which(df1$mes == "ago"),c("ago")] <- 1
  df1[which(df1$mes == "set"),c("set")] <- 1
  df1[which(df1$mes == "out"),c("out")] <- 1
  df1[which(df1$mes == "nov"),c("nov")] <- 1
  df1[which(df1$mes == "dez"),c("dez")] <- 1
  
  
  # Ending 
  assign(nomes[i], df1)
  rm(df1)
}

################################################
################## DB END INFO #################
################################################

### Organizing DB and adding promotiong, BA AF feriado

# Sorting by date
for (i in 1:length(nomes)){
  df1 <- get(nomes[i])
  df1 <- df1[order(as.Date(df1$poxicct_dia, format = "%Y/%m/%d")),]
  df1$before_fer <- 0
  df1$after_fer <- 0
  df1$promocao <- 0
  
  # 0 to NA in specific variables
  df1$vendas[which(df1$vendas == 0)] <- NA
  df1$entradas[which(df1$entradas == 0)] <- NA
  df1$quebras[which(df1$quebras == 0)] <- NA
  
  # day as poxicct
  df1$poxicct_dia <- as.POSIXct(df1$poxicct_dia)
  
  # as DF
  df1 <- data.frame(df1)
  
  # Before feriado
  for (f in 1:length(dias_feriado)) {
    df1$before_fer[as.numeric(which(df1$dia == dias_feriado[f]) -1)] <- 1
  }
  
  # After feriado
  for (f in 1:length(dias_feriado)) {
    df1$after_fer[as.numeric(which(df1$dia == dias_feriado[f]) +1)] <- 1
  }
  
  #Promotion dummy
  for (f in 2:nrow(df1)) {
    df1$promocao[f] <- ifelse(df1[f-1,c("preco_de_venda")] > df1[f,c("preco_de_venda")] & df1[f+1,c("preco_de_venda")] <= df1[f+2,c("preco_de_venda")],df1$promocao[f] <- 1,df1$promocao[f] <- 0)
  }
  
  # Ending
  assign(nomes[i],df1)
  rm(df1)
  
}

## Saving completed DB
for (i in 1:length(nomes)){
  write.csv(get(nomes[i]),paste0('G:/Meu Drive/Tactin/',
                                 'Trabalhos - Análise de Dados/Serv bem/',
                                 'R-project/R/Dados/10 - 10 - 2019/',
                                 'Completas/[COMPLETA]',nomes[i],".csv"), row.names = FALSE)
}

######## END CODE ########

