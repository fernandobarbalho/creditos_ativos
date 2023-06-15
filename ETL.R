library(readr)
library(tidyverse)
library(janitor)


arquivo_lai_SIDA_202303<-
  purrr::map_dfr(1:6, function(parte){
    print(parte)
    nome_arq<- paste0("Dados_abertos_Nao_Previdenciario/arquivo_lai_SIDA_",parte,"_202303.csv")
    read_delim(nome_arq,
               delim = ";", escape_double = FALSE, col_types = cols(NUMERO_INSCRICAO = col_character(), DATA_INSCRICAO = col_date(format = "%d/%m/%Y")),
               locale = locale(encoding = "latin1"),
               trim_ws = TRUE)


  })


arquivo_lai_SIDA_202303<- janitor::clean_names(arquivo_lai_SIDA_202303)



saveRDS(arquivo_lai_SIDA_202303,"arquivo_lai_SIDA_202303.RDS")



