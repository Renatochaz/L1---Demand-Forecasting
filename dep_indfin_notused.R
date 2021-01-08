# DONT CLEAN
# depreciacao indicador financeiro - special case, DONT CLEAN
{
  depreciacao_amortizacao_indfin <- read.csv("depreciacao_amortizacao_indicador_fin.csv", stringsAsFactors = FALSE)
  
  epreciacao_amortizacao_indfin$Data <- as.Date(depreciacao_amortizacao_indfin$Data, "%Y-%m-%d")
  
  str(depreciacao_amortizacao_indfin)
  
  depreciacao_amortizacao_indfin <- tidy_split4(depreciacao_amortizacao_indfin)
  
  match(colnames(depreciacao_amortizacao_indfin)[3:463], colnames(ativo_total2)[3:463])
  
  colnames(depreciacao_amortizacao_indfin)[3:463] <- gsub(".", " ", colnames(depreciacao_amortizacao_indfin)[3:463], fixed = TRUE)
  
  match(colnames(depreciacao_amortizacao_indfin)[3:463], colnames(ativo_total2)[3:463])
  
  which(is.na(match(colnames(depreciacao_amortizacao_indfin)[3:463], colnames(ativo_total2)[3:463])))
  
  colnames(depreciacao_amortizacao_indfin)[185]
  colnames(ativo_total2)[185]
  
  colnames(depreciacao_amortizacao_indfin)[248]
  colnames(ativo_total2)[248]
  
  colnames(depreciacao_amortizacao_indfin)[408]
  colnames(ativo_total2)[408]
  
  
  length (which(is.na(depreciacao_amortizacao_indfin) == TRUE))
  colnames(depreciacao_amortizacao_indfin) <- colnames(ativo_total2)
  
  depreciacao_amortizacao_indfin <- depreciacao_amortizacao_indfin[ ,-cols]
  
  check_consistency(depreciacao_amortizacao_indfin)
  
  write.csv(depreciacao_amortizacao_indfin,"depreciacao_indfin.csv")
  
  deprec_indfin <- read.csv("depreciacao_indfin.csv")
  
  str(deprec_indfin)
}