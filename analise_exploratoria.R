####Análise dados mais atuais


arquivo_lai_SIDA_202303 <- readRDS("~/Github/creditos_ativos/arquivo_lai_SIDA_202303.RDS")



arquivo_lai_SIDA_202303 %>%
  group_by(tipo_pessoa) %>%
  summarise(
    n()
  )

glimpse(arquivo_lai_SIDA_202303)

arquivo_lai_SIDA_202303 %>%
  group_by(uf_devedor) %>%
  summarise(
    quantidade =  n()
  ) %>%
  arrange(desc(quantidade))


arquivo_lai_SIDA_202303 %>%
  group_by(tipo_situacao_inscricao) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))


arquivo_lai_SIDA_202303 %>%
  group_by(situacao_inscricao) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))



arquivo_lai_SIDA_202303 %>%
  group_by(receita_principal) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado),
    media_valor_consolidado = mean(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))



corte_temporal_pf<-
  arquivo_lai_SIDA_202303 %>%
  filter(tipo_devedor == "PRINCIPAL",
         tipo_pessoa == "Pessoa física") %>%
  mutate(ano = lubridate::year(data_inscricao)) %>%
  group_by(ano) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))


corte_temporal_pj<-
  arquivo_lai_SIDA_202303 %>%
  filter(tipo_devedor == "PRINCIPAL",
         tipo_pessoa == "Pessoa jurídica") %>%
  mutate(ano = lubridate::year(data_inscricao)) %>%
  group_by(ano) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))




####Análise longitudinal


arquivo_lai_SIDA_202303 <- readRDS("~/Github/creditos_ativos/arquivo_lai_SIDA_202303.RDS")

arquivo_lai_SIDA_202203 <- readRDS("~/Github/creditos_ativos/arquivo_lai_SIDA_202203.RDS")

dados_longitudinais<-
  arquivo_lai_SIDA_202203 %>%
  anti_join(arquivo_lai_SIDA_202303, by="numero_inscricao")

dados_longitudinais_trabalho<-
  dados_longitudinais%>%
  filter(tipo_devedor=="PRINCIPAL")

dados_longitudinais_trabalho$data_max_referencia<-as.Date("2023-03-31")


# Instale a biblioteca "lubridate" caso ainda não a tenha instalado
# install.packages("lubridate")

# Carregue a biblioteca "lubridate"
library(lubridate)



# Calcule a diferença entre as datas em meses usando a função "interval()" do "lubridate"
dados_longitudinais_trabalho$diferenca_max_meses <- interval(dados_longitudinais_trabalho$data_inscricao,
                                                             dados_longitudinais_trabalho$data_max_referencia) / months(1)


dados_longitudinais_trabalho$diferenca_max_meses<- round(dados_longitudinais_trabalho$diferenca_max_meses,0)

saveRDS(dados_longitudinais_trabalho,"dados_longitudinais_trabalho.RDS")

set.seed(1972)
#Dataframe incluindo dados censurados
dados_censurados<-
  arquivo_lai_SIDA_202203 %>%
  filter(tipo_devedor=="PRINCIPAL") %>%
  slice_sample(prop = 0.3) %>%
  anti_join(dados_longitudinais_trabalho, by="numero_inscricao")


dados_censurados$data_max_referencia<-as.Date("2023-03-31")


dados_censurados$diferenca_max_meses <- interval(dados_censurados$data_inscricao,
                                                 dados_censurados$data_max_referencia) / months(1)


dados_censurados$diferenca_max_meses<- round(dados_censurados$diferenca_max_meses,0)

dados_censurados$status<-0

dados_longitudinais_trabalho_full<-
  dados_censurados %>%
  bind_rows(
    dados_longitudinais_trabalho %>%
      mutate(status=1) %>%
      slice_sample(prop =.21)
  )


dados_longitudinais_trabalho_full$data_max_referencia<-as.Date("2023-03-31")


# Instale a biblioteca "lubridate" caso ainda não a tenha instalado
# install.packages("lubridate")

# Carregue a biblioteca "lubridate"
library(lubridate)



# Calcule a diferença entre as datas em meses usando a função "interval()" do "lubridate"
dados_longitudinais_trabalho_full$diferenca_max_meses <- interval(dados_longitudinais_trabalho_full$data_inscricao,
                                                                  dados_longitudinais_trabalho_full$data_max_referencia) / months(1)

glimpse(dados_longitudinais_trabalho_full)

saveRDS(dados_longitudinais_trabalho_full,"dados_longitudinais_trabalho_full.RDS")

dados_longitudinais_trabalho_full %>%
  group_by(status) %>%
  summarise(n())
