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


top_cnae_sections<-
  (creditos_ativos %>%
  mutate(cnae_secao_descr = ifelse(cnae_secao_descr=="N/A","Pessoa Física",stringr::str_to_title(cnae_secao_descr) )) %>%
  group_by(cnae_secao_descr) %>%
  summarise(
    vl_total = sum(vl_total)
  ) %>%
  slice_max(vl_total, n=8))$cnae_secao_descr

top_tax_groups<-
  (creditos_ativos %>%
     group_by(grupo_tributo) %>%
     summarise(
       vl_total = sum(vl_total)
     ) %>%
     slice_max(vl_total, n=8))$grupo_tributo

grouped<-
  creditos_ativos %>%
  mutate(cnae_secao_descr = ifelse(cnae_secao_descr=="N/A","Pessoa Física",stringr::str_to_title(cnae_secao_descr) )) %>%
  filter(cnae_secao_descr %in% top_cnae_sections,
         grupo_tributo %in% top_tax_groups) %>%
  group_by(cnae_secao_descr, grupo_tributo) %>%
  summarise(
    vl_total = sum(vl_total)
  ) %>%
  ungroup()

# Load the necessary library
library(networkD3)

# Define the nodes
nodes <- data.frame(posicao=0:15, name = c(as.character(top_cnae_sections), as.character(top_tax_groups)))

source_obj<-
  grouped %>%
  rename(name = cnae_secao_descr) %>%
  inner_join(nodes) %>%
  select(posicao)


target_obj<-
  grouped %>%
  rename(name = grupo_tributo) %>%
  inner_join(nodes) %>%
  select(posicao)


# Define the links
links <- data.frame(
  source = source_obj$posicao, # -1 because R is 1-indexed while Python is 0-indexed
  target = target_obj$posicao,
  value = grouped$vl_total
)





# Create the Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              fontSize = 12, nodeWidth = 30)



grouped<-
  creditos_ativos %>%
  mutate(cnae_secao_descr = ifelse(cnae_secao_descr=="N/A","Pessoa Física",stringr::str_to_title(cnae_secao_descr) )) %>%
  filter(grupo_tributo %in% top_tax_groups) %>%
  group_by(situacao, grupo_tributo) %>%
  summarise(
    vl_total = sum(vl_total)
  ) %>%
  ungroup()


# Define the nodes
nodes <- data.frame(posicao=0:11, name = c(top_tax_groups, unique(creditos_ativos$situacao)))

source_obj<-
  grouped %>%
  rename(name = grupo_tributo) %>%
  inner_join(nodes) %>%
  select(posicao)


target_obj<-
  grouped %>%
  rename(name = situacao) %>%
  inner_join(nodes) %>%
  select(posicao)


# Define the links
links <- data.frame(
  source = source_obj$posicao, # -1 because R is 1-indexed while Python is 0-indexed
  target = target_obj$posicao,
  value = grouped$vl_total
)





# Create the Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              fontSize = 12, nodeWidth = 30)
