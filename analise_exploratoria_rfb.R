library(forecast)
library(lubridate)


#Função para deflacionar valores usando o IPCA

calcula_valor_constante <- function(df_dados_hist, data_constante ){
  #Argumentos
  #df_dados_hist: dataframe em que as duas primeiras colunas são formadas por uma data (Y-m-d) e um valor
  #data_constante: data para gerar valor constante. Formato da data: Y-m-d

  #Por enquanto as datas estão limitadas à da série temporal baixada do IPCA Total do portal de dados abertos do BACEN
  #Essa série começa em 1992-01-01

  library(dplyr)
  library(lubridate)

  library(httr)
  library(jsonlite)


  names(df_dados_hist)[1:2]<- c("data","valor")

  ###faz a conversão para real
  df_dados_hist<-
    df_dados_hist %>%
    mutate(valor= case_when(
      data>= ymd("1992-01-01") & data<= ymd("1993/07/31") ~ valor/(1000^2*2.75),
      data>= ymd("1993-08-01") & data<= ymd("1994/06/30") ~ valor/(1000*2.75),
      TRUE ~ valor
    ))

  #################Cálculos econômicos
  #Dados do IPCA com a série temporal

  url<- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4449/dados?formato=json"


  resposta <- GET(url)
  IPCA_historico <- content(resposta, "text")
  IPCA_historico <- fromJSON(IPCA_historico, flatten = TRUE)


  IPCA_historico$valor <- as.numeric(IPCA_historico$valor)

  # IPCA_historico <- read_delim("bcdata.sgs.4449.csv",
  #                              ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
  #                                                                          grouping_mark = "."), trim_ws = TRUE)
  IPCA_historico<-
    IPCA_historico %>%
    filter(data<= ymd(data_constante))

  #################Cálculos econômicos
  #Calcula o número índice para todos os meses da série temporal
  num_index<-
    map_dbl(1:NROW(IPCA_historico), function(a_i){
      if (a_i==1){
        100
      }else {
        100+ prod(1+IPCA_historico$valor[1:a_i]/100)
      }

    })

  IPCA_historico$num_indice <- num_index

  num_indice_constante<-
    (IPCA_historico%>%
       filter(dmy(data)== ymd(data_constante)))$num_indice


  #################Cálculos econômicos
  #Acrescenta a coluna do número índice
  IPCA_historico$num_indice <- num_index

  IPCA_historico_trab<-
    IPCA_historico %>%
    mutate(data= dmy(data))  %>%
    filter(data %in% df_dados_hist$data) %>%
    select(data,num_indice)

  df_dados_hist %>%
    inner_join(IPCA_historico_trab) %>%
    mutate(valor = valor * (num_indice_constante/num_indice)) %>%
    select(data, valor)

}





####Análise de séries temporais


###Análise das quantidades

contencioso_administrativo_de_primeira_instancia %>%
  #dplyr::filter(str_sub(mes_ano,5,6) == "22" ) %>%
  ggplot() +
  geom_line(aes(x=data, y= quantidade_de_processos), color="black") +
  theme(
    panel.grid = element_blank(),

  )



serie_temporal_quantidade <- ts(contencioso_administrativo_de_primeira_instancia$quantidade_de_processos,
                     frequency = 12,
                     start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                               month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_quantidade)

autoplot(decomp)



###análise da série temporal dos valores


#Valores históricos

serie_temporal_valor_total_hist <- ts(contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos,
                                 frequency = 12,
                                 start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                           month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_total_hist)

autoplot(decomp)



serie_temporal_valor_julgado_hist <- ts(contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos_julgados,
                                        frequency = 12,
                                        start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                                  month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_julgado_hist)

autoplot(decomp)



df_dados_hist<-
  tibble(data=contencioso_administrativo_de_primeira_instancia$data,
         valor =contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos)

df_valores_constantes<- calcula_valor_constante(df_dados_hist, "2023-05-01" )



serie_temporal_valor_total <- ts(df_valores_constantes$valor,
                                frequency = 12,
                                start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                          month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_total)

autoplot(decomp)


df_dados_hist<-
  tibble(data=contencioso_administrativo_de_primeira_instancia$data,
         valor =contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos_julgados)

df_valores_constantes<- calcula_valor_constante(df_dados_hist, "2023-05-01"  )



serie_temporal_valor_julgado <- ts(df_valores_constantes$valor,
                                 frequency = 12,
                                 start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                           month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_julgado)

autoplot(decomp)


###Análise dos tempos de julgamento

contencioso_administrativo_de_primeira_instancia %>%
  select(data, starts_with("tempo")) %>%
  pivot_longer(cols=-data, names_to = "tempo", values_to = "valor") %>%
  ggplot() +
  geom_line(aes(x=data, y= valor)) +
  theme_light() +
  facet_wrap(tempo~.)
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  )
