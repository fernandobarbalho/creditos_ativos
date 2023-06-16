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

dados_longitudinais_trabalho$data_max_referencia<-as.Date("2022-12-31")

diferenca_meses <- as.numeric(difftime(as.Date("2022-12-31"), as.Date("2022-12-01"), units = "months"))

diferenca_meses <- as.numeric(difftime(dados$data_fim, dados$data_inicio, units = "months"))

# Instale a biblioteca "lubridate" caso ainda não a tenha instalado
# install.packages("lubridate")

# Carregue a biblioteca "lubridate"
library(lubridate)

# Carregue a biblioteca "lubridate"
library(lubridate)


# Converta as colunas de data para o formato de data usando a função "ymd()" do "lubridate"
dados$data_inicio <- ymd(dados$data_inicio)
dados$data_fim <- ymd(dados$data_fim)

# Calcule a diferença entre as datas em meses usando a função "interval()" do "lubridate"
dados_longitudinais_trabalho$diferenca_max_meses <- interval(dados_longitudinais_trabalho$data_inscricao,
                                                             dados_longitudinais_trabalho$data_max_referencia) / months(1)




