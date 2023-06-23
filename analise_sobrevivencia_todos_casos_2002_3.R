##Análise de sobrevivência

# Instale os pacotes necessários
#install.packages("survival")
#install.packages("survminer")

# Carregue os pacotes
library(survival)
library(survminer)
library(questionr)







# Crie um objeto de sobrevivência utilizando a função Surv() do pacote survival
sobrevivencia <- Surv(time = dados_longitudinais_trabalho_full_full$diferenca_max_meses,
                      event = dados_longitudinais_trabalho_full_full$status)


############## Análises de sobrevivência com grupos para tipo de pessoa

dados_longitudinais_trabalho_full %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado, fill = tipo_pessoa), alpha=0.5) +
  scale_x_log10()


# Realize a comparação de grupos usando o teste de log-rank
survdiff(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho_full)



# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste <- survfit(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho_full)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste, data = dados_longitudinais_trabalho_full,  conf.int = TRUE, censor=FALSE, surv.median.line= "hv", risk.table = TRUE)



############## Análises de sobrevivência com grupos para indicador ajuizado

dados_longitudinais_trabalho_full %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado, fill = indicador_ajuizado), alpha=0.5) +
  scale_x_log10()



# Realize a comparação de grupos usando o teste de log-rank
survdiff(sobrevivencia ~ indicador_ajuizado, data = dados_longitudinais_trabalho_full)



# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_ajuizado <- survfit(sobrevivencia ~ indicador_ajuizado, data = dados_longitudinais_trabalho_full)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_ajuizado, data = dados_longitudinais_trabalho_full,  conf.int = TRUE, censor=FALSE, surv.median.line= "hv", risk.table = TRUE)


############## Análises de sobrevivência com grupos para tipo_situacao_inscricao



unique(dados_longitudinais_trabalho_full$tipo_situacao_inscricao)

dados_longitudinais_trabalho_full %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado, fill = tipo_situacao_inscricao), alpha=0.5) +
  scale_x_log10()


# Realize a comparação de grupos usando o teste de log-rank
survdiff(sobrevivencia ~ tipo_situacao_inscricao, data = dados_longitudinais_trabalho_full)

# Exiba o resultado do teste
summary(comparacao_situacao)


# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_situacao <- survfit(sobrevivencia ~ tipo_situacao_inscricao, data = dados_longitudinais_trabalho_full)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_situacao, data = dados_longitudinais_trabalho_full, conf.int = TRUE, censor=FALSE, surv.median.line= "hv", risk.table = TRUE)



########## Situacao_inscricao
questionr::freq(dados_longitudinais_trabalho_full$situacao_inscricao, cum = TRUE, sort = "dec", total = TRUE)

dados_longitudinais_trabalho_full %>%
  group_by(situacao_inscricao) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado),
    media_valor_consolidado = mean(valor_consolidado),
    mediana_valor_consolidadeo = median(valor_consolidado)
  ) %>%
  slice_max(order_by = valor_consolidado_total, n=10)


top_4_situacao_inscricao<-
  (dados_longitudinais_trabalho_full %>%
  group_by(situacao_inscricao) %>%
  summarise(
    quantidade =  n()
  ) %>%
  slice_max(order_by = quantidade, n=4))$situacao_inscricao



dados_longitudinais_trabalho_full %>%
  filter(situacao_inscricao %in% top_4_situacao_inscricao) %>%
  mutate(status = as.character(status)) %>%
  group_by(situacao_inscricao, status) %>%
  summarise(n())



dados_longitudinais_trabalho_full %>%
  filter(situacao_inscricao %in% top_4_situacao_inscricao) %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado, fill = situacao_inscricao), alpha=0.5) +
  scale_x_log10()


trabalho_situacao_inscricao<-
dados_longitudinais_trabalho_full %>%
  filter(situacao_inscricao %in% top_4_situacao_inscricao)

sobrevivencia_situacao_inscricao <- Surv(time = trabalho_situacao_inscricao$diferenca_max_meses,
                                         event = trabalho_situacao_inscricao$status)

# Realize a comparação de grupos usando o teste de log-rank
survdiff(sobrevivencia_situacao_inscricao ~ situacao_inscricao, data = trabalho_situacao_inscricao)



# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_situacao_inscricao <- survfit(sobrevivencia_situacao_inscricao ~ situacao_inscricao, data = trabalho_situacao_inscricao)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_situacao_inscricao, data = trabalho_situacao_inscricao, conf.int = TRUE, censor=FALSE, surv.median.line= "hv", risk.table = TRUE)


########## receita_principal

dados_longitudinais_trabalho_full %>%
  group_by(receita_principal) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado),
    media_valor_consolidado = mean(valor_consolidado),
    mediana_valor_consolidado = median(valor_consolidado)
  ) %>%
  slice_max(order_by = valor_consolidado_total, n=10)

questionr::freq(dados_longitudinais_trabalho_full$receita_principal, cum = TRUE, sort = "dec", total = TRUE)


top_8_receita_principal<-
  (dados_longitudinais_trabalho_full %>%
     group_by(receita_principal) %>%
     summarise(
       quantidade =  n()
     ) %>%
     slice_max(order_by = quantidade, n=8))$receita_principal

dados_longitudinais_trabalho_full %>%
  filter(receita_principal %in% top_8_receita_principal) %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado)) +
  scale_x_log10() +
  facet_wrap(receita_principal~.)


trabalho_receita_principal<-
  dados_longitudinais_trabalho_full %>%
  filter(receita_principal %in% top_8_receita_principal)

sobrevivencia_receita_principal <- Surv(time = trabalho_receita_principal$diferenca_max_meses,
                                        event = trabalho_receita_principal$status)

# Realize a comparação de grupos usando o teste de log-rank
survdiff(sobrevivencia_receita_principal ~ receita_principal,
                                          data = trabalho_receita_principal)



# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_receita_principal <- survfit(sobrevivencia_receita_principal ~ receita_principal, data = trabalho_receita_principal)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_receita_principal, data = trabalho_receita_principal, risk.table = TRUE)


##### Valor consolidado

dados_longitudinais_trabalho_full %>%
  ggplot() +
  geom_density(aes(x= valor_consolidado)) +
  scale_x_log10()


