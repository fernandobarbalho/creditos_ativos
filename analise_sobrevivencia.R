##Análise de sobrevivência

# Instale os pacotes necessários
#install.packages("survival")
#install.packages("survminer")

# Carregue os pacotes
library(survival)
library(survminer)
library(questionr)



dados_longitudinais_trabalho$diferenca_max_meses<- round(dados_longitudinais_trabalho$diferenca_max_meses,0)


# Crie um objeto de sobrevivência utilizando a função Surv() do pacote survival
sobrevivencia <- Surv(time = dados_longitudinais_trabalho$diferenca_max_meses)


############## Análises de sobrevivência com grupos para tipo de pessoa

# Realize a comparação de grupos usando o teste de log-rank
comparacao <- survdiff(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho)

# Exiba o resultado do teste
summary(comparacao)


# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste <- survfit(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste, data = dados_longitudinais_trabalho, risk.table = TRUE)



############## Análises de sobrevivência com grupos para tipo ajuizado

# Realize a comparação de grupos usando o teste de log-rank
comparacao_ajuizado <- survdiff(sobrevivencia ~ indicador_ajuizado, data = dados_longitudinais_trabalho)

# Exiba o resultado do teste
summary(comparacao_ajuizado)


# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_ajuizado <- survfit(sobrevivencia ~ indicador_ajuizado, data = dados_longitudinais_trabalho)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_ajuizado, data = dados_longitudinais_trabalho, risk.table = TRUE)


############## Análises de sobrevivência com grupos para situacao_inscricao

unique(dados_longitudinais_trabalho$tipo_situacao_inscricao)

# Realize a comparação de grupos usando o teste de log-rank
comparacao_situacao <- survdiff(sobrevivencia ~ tipo_situacao_inscricao, data = dados_longitudinais_trabalho)

# Exiba o resultado do teste
summary(comparacao_situacao)


# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste_situacao <- survfit(sobrevivencia ~ tipo_situacao_inscricao, data = dados_longitudinais_trabalho)


# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste_situacao, data = dados_longitudinais_trabalho, risk.table = TRUE)



########## Situacao_inscricao
dados_longitudinais_trabalho %>%
  group_by(situacao_inscricao) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado),
    media_valor_consolidado = mean(valor_consolidado),
    mediana_valor_consolidadeo = median(valor_consolidado)
  ) %>%
  slice_max(order_by = valor_consolidado_total, n=10)



questionr::freq(dados_longitudinais_trabalho$situacao_inscricao, cum = TRUE, sort = "dec", total = TRUE)

########## receita_principal

dados_longitudinais_trabalho %>%
  group_by(receita_principal) %>%
  summarise(
    quantidade =  n(),
    valor_consolidado_total = sum(valor_consolidado),
    media_valor_consolidado = mean(valor_consolidado),
    mediana_valor_consolidado = median(valor_consolidado)
  ) %>%
  slice_max(order_by = valor_consolidado_total, n=10)

questionr::freq(dados_longitudinais_trabalho$receita_principal, cum = TRUE, sort = "dec", total = TRUE)


