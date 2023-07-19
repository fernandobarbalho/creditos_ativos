library(tidyverse)
library(plotly)


df_dados_hist<-
  tibble(data= creditos_ativos$data,
         valor = creditos_ativos$vl_total)

df_valores_constantes<- calcula_valor_constante(df_dados_hist, "2023-05-01" )


creditos_ativos$valor_constante<- df_valores_constantes$valor

creditos_ativos %>%
  group_by(data) %>%
  summarise(valor = sum(valor_constante)) %>%
  ggplot() +
  geom_line(aes(x=data, y= valor/10^9), color="black") +
  theme_light()+
  theme(
    panel.grid = element_blank(),

  ) +
  labs(
    title =  "Evolução de valores totais",
    subtitle = "Valores atualizados pelo IPCA",
    y= "Valores totais em R$(bi)",
    x= ""
  )


grafico<-
creditos_ativos %>%
  group_by(data) %>%
  summarise(valor = sum(vl_total)) %>%
  ggplot() +
  geom_line(aes(x=data, y= valor/10^9), color="black") +
  theme_light()+
  theme(
    panel.grid = element_blank(),

  ) +
  labs(
    title =  "Evolução de valores totais",
    subtitle = "Valores atualizados pelo IPCA",
    y= "Valores totais em R$(bi)",
    x= ""
  )

plotly::ggplotly(grafico)
