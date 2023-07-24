library(tidyverse)
library(plotly)
library(ggrepel)


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
  ylim(0, 3000) +
  labs(
    title =  "Evolução de valores totais",
    subtitle = "Valores atualizados pelo IPCA",
    y= "Valores totais em R$(bi)",
    x= ""
  )


creditos_ativos %>%
  group_by(data) %>%
  summarise(valor = sum(vl_total)) %>%
  ggplot() +
  geom_line(aes(x=data, y= valor/10^9), color="black") +
  theme_light()+
  theme(
    panel.grid = element_blank(),

  ) +
  ylim(0, 3000) +
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
  ylim(0, 3000) +
  labs(
    title =  "Evolução de valores totais",
    subtitle = "Valores atualizados pelo IPCA",
    y= "Valores totais em R$(bi)",
    x= ""
  )

plotly::ggplotly(grafico)


fab_zero<-
  creditos_ativos %>%
  filter(data == "2023-02-01",
         vl_total <= 1)


fab_fevereiro<-
  creditos_ativos %>%
  filter(data == "2023-02-01")


fab_marco<-
  creditos_ativos %>%
  filter(data == "2023-03-01")


creditos_ativos %>%
  #filter(data %in% c(as.Date("2023-02-01") , as.Date("2023-03-01"))) %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),vl_total)) +
  scale_y_log10()


creditos_ativos %>%
  #filter(data %in% c(as.Date("2023-02-01") , as.Date("2023-03-01"))) %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),vl_total))



creditos_ativos %>%
  filter(data %in% c(as.Date("2023-02-01") , as.Date("2023-03-01"))) %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),vl_total)) +
  scale_y_log10()


max_valor<-
  creditos_ativos %>%
  filter(data <= "2023-02-01" ) %>%
  group_by(data) %>%
  summarise(
    vl_total = max(vl_total)
  ) %>%
  inner_join(
    creditos_ativos %>%
      select(data, vl_total,uf, tipo_contribuinte, situacao)
  )

creditos_ativos %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),y = vl_total/10^9)) +
  geom_text_repel (data= max_valor,
                   aes(x=as.character(data),
                       y= vl_total/10^9,
                       label= paste(uf, tipo_contribuinte,  situacao, sep = "-")),
                   size=2) +
  theme_light()+
  labs(y= "Valores em R$ bi",
       x="",
       title= "Distribuição dos valores de créditos ativos")



creditos_ativos %>%
  #filter(data %in% c(as.Date("2023-02-01") , as.Date("2023-03-01"))) %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),vl_total)) +
  scale_y_log10()

creditos_ativos %>%
  ggplot() +
  geom_boxplot(aes(x=as.character(data),y = vl_total)) +
  geom_text_repel (data= max_valor,
                   aes(x=as.character(data),
                       y= vl_total,
                       label= paste(uf, tipo_contribuinte,  situacao, sep = "-")),
                   size=2) +
  theme_light()+
  labs(y= "Valores em R$ bi",
       x="",
       title= "Distribuição dos valores de créditos ativos") +
  scale_y_log10()


IPCA_historico %>%
  ggplot() +
  geom_line(aes(x=as.Date(paste(str_sub(data,7,10),str_sub(data,4,5),str_sub(data,1,2), sep = "-") ),y=valor)) +
  labs(x="")
