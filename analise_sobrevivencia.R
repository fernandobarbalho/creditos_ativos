##Análise de sobrevivência

# Instale os pacotes necessários
install.packages("survival")
install.packages("survminer")

# Carregue os pacotes
library(survival)
library(survminer)

# Carregue os dados do arquivo CSV

# Crie um objeto de sobrevivência utilizando a função Surv() do pacote survival
sobrevivencia <- Surv(time = dados_longitudinais_trabalho$diferenca_max_meses)

# Realize a comparação de grupos usando o teste de log-rank
comparacao <- survdiff(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho)

# Ajuste os modelos de sobrevivência separadamente para cada grupo
ajuste <- survfit(sobrevivencia ~ tipo_pessoa, data = dados_longitudinais_trabalho)

# Exiba o resultado do teste
summary(comparacao)

# Plote as curvas de sobrevivência para cada grupo
ggsurvplot(ajuste, data = dados_longitudinais_trabalho, risk.table = TRUE)
