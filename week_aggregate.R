library(lubridate)
library(xts)

df1_xts <- as.xts(`alface crespa`$vendas, order.by=as.Date(`alface crespa`$poxicct_dia))
df1_xts <- apply.weekly(df1_xts, sum)

alf_cresp_week <- data.frame(df1_xts)
alf_cresp_week$week <- 1:nrow(alf_cresp_week)
alf_cresp_week <- alf_cresp_week[,c(2,1)]
alf_cresp_week <- alf_cresp_week %>%
  rename(
    vendas = 2
  )



"alf_cresp_week <- `alface crespa`[c(28:872),1:30]
alf_cresp_week$semana <- 0
rownames(alf_cresp_week) <- 1:nrow(alf_cresp_week)
count_week <- 1

for (i in 1:141) {
  
  for (w in 1:6) {
    
    if (alf_cresp_week$dia_semana[count_week] == "seg"){
        ifelse(alf_cresp_week$dia_semana[count_week - 1] == "sáb" & alf_cresp_week$dia_semana[count_week - 2] == "sex",
               alf_cresp_week$semana[count_week] <- i,
               ifelse(alf_cresp_week$dia_semana[count_week - 1] == "ter",
                      (alf_cresp_week$semana[count_week] <- i + 1) & (count_week <- count_week +1) & break,"erro"))
        count_week <- count_week +1
        
    } else if (alf_cresp_week$dia_semana[count_week] == "ter"){
      ifelse(alf_cresp_week$dia_semana[count_week - 1] == "seg",
             alf_cresp_week$semana[count_week] <- i,
             i-1)
        count_week <- count_week +1
      
    } else if (alf_cresp_week$dia_semana[count_week] == "qua"){
      ifelse(alf_cresp_week$dia_semana[count_week - 1] == "ter",
             alf_cresp_week$semana[count_week] <- i,
             i-1)
        count_week <- count_week +1
      
    } else if (alf_cresp_week$dia_semana[count_week] == "qui"){
        ifelse(alf_cresp_week$dia_semana[count_week - 1] == "qua",
                alf_cresp_week$semana[count_week] <- i,
               ifelse(alf_cresp_week$dia_semana[count_week - 1] == "ter",
                      (alf_cresp_week$semana[count_week] <- i) & (count_week <- count_week +1) & next,
                      alf_cresp_week$semana[count_week] <- "erro"))
        count_week <- count_week +1
      
    } else if (alf_cresp_week$dia_semana[count_week] == "sex"){
      ifelse(alf_cresp_week$dia_semana[count_week - 1] == "qui",
             alf_cresp_week$semana[count_week] <- i,
             ifelse(alf_cresp_week$dia_semana[count_week - 1] == "qua",
                    (alf_cresp_week$semana[count_week] <- i) & (count_week <- count_week +1) & next,alf_cresp_week$semana[count_week] <- "erro"))
        count_week <- count_week +1
      
    } else if (alf_cresp_week$dia_semana[count_week] == "sáb"){
      ifelse(alf_cresp_week$dia_semana[count_week - 3] == "ter",
             (alf_cresp_week$semana[count_week] <- i) & (count_week <- count_week +1) & break, ifelse(alf_cresp_week$dia_semana[count_week - 2] == "qua",
             (alf_cresp_week$semana[count_week] <- i) & (count_week <- count_week +1) & break,
             ifelse(alf_cresp_week$dia_semana[count_week - 1] == "sex", 
                    alf_cresp_week$semana[count_week] <- i,alf_cresp_week$semana[count_week] <- erro)))
        count_week <- count_week +1
      
    } 
    
  }
  
}
#
