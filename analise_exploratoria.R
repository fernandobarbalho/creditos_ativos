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




head_sample<-
  arquivo_lai_SIDA_202303 %>%
  slice_head(n=1000)

