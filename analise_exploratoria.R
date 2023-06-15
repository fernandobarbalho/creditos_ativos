arquivo_lai_SIDA_202303 <- readRDS("~/Github/creditos_ativos/arquivo_lai_SIDA_202303.RDS")

summary(arquivo_lai_SIDA_202303)


NROW(unique(arquivo_lai_SIDA_202303$numero_inscricao))


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


fab<-
  arquivo_lai_SIDA_202303 %>%
  mutate(ano = lubridate::year(data_inscricao)) %>%
  slice_head(n=10)


arquivo_lai_SIDA_202303 %>%
  mutate(ano = lubridate::year(data_inscricao)) %>%
  group_by(ano) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado)
  ) %>%
  arrange(desc(quantidade))
