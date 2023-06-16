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


uf_siglas <- c("AC",  "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

uf_excluir <- c("AL", "CE", "PB")

uf_siglas_filtradas <- uf_siglas[!uf_siglas %in% uf_excluir]

arquivo_lai_SIDA_202303<-
  purrr::map_dfr(uf_siglas_filtradas, function(uf){
    print(uf)
    nome_arq<- paste0("Dados_abertos_Nao_Previdenciario_3_2022/arquivo_lai_SIDA_",uf,"_202203.csv")
    read_delim(nome_arq,
               delim = ";", escape_double = FALSE, col_types = cols(NUMERO_INSCRICAO = col_character(),
                                                                    DATA_INSCRICAO = col_date(format = "%d/%m/%Y")),
               locale = locale(encoding = "latin1"),
               trim_ws = TRUE)

  })


arquivo_lai_SIDA_202303<- janitor::clean_names(arquivo_lai_SIDA_202303)


uf_siglas <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")



saveRDS(arquivo_lai_SIDA_202303,"arquivo_lai_SIDA_202303.RDS")



