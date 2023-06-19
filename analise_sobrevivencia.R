##Análise de sobrevivência

# Instale os pacotes necessários
install.packages("survival")
install.packages("survminer")

# Carregue os pacotes
library(survival)
library(survminer)



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

