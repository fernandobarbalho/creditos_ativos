library(forecast)
library(lubridate)




contencioso_administrativo_de_primeira_instancia %>%
  dplyr::filter(str_sub(mes_ano,5,6) >= "21" ) %>%
  ggplot() +
  geom_col(aes(x=mes_ano, y= quantidade_de_processos), color="white") +
  theme(
    panel.grid = element_blank()
  )


contencioso_administrativo_de_primeira_instancia %>%
  dplyr::filter(str_sub(mes_ano,5,6) == "22" ) %>%
  ggplot() +
  geom_line(aes(x=mes_ano, y= quantidade_de_processos), color="black") +
  theme(
    panel.grid = element_blank(),

  )



contencioso_administrativo_de_primeira_instancia$data<-
  str_c("01-",contencioso_administrativo_de_primeira_instancia$mes_ano)

contencioso_administrativo_de_primeira_instancia$data <-
  as.Date(contencioso_administrativo_de_primeira_instancia$data, format = "%d-%B-%y")

serie_temporal_quantidade <- ts(contencioso_administrativo_de_primeira_instancia$quantidade_de_processos,
                     frequency = 12,
                     start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                               month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal)

autoplot(decomp)



calcula_valor_constante <- function(df_dados_hist, data_constante ){
  #Argumentos
  #df_dados_hist: dataframe em que as duas primeiras colunas são formadas por uma data (Y-m-d) e um valor
  #data_constante: data para gerar valor constante. Formato da data: Y-m-d

  #Por enquanto as datas estão limitadas à da série temporal baixada do IPCA Total do portal de dados abertos do BACEN
  #Essa série começa em 1992-01-01

  library(dplyr)
  library(lubridate)

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

  download.file("http://api.bcb.gov.br/dados/serie/bcdata.sgs.4449/dados?formato=csv", destfile = "bcdata.sgs.4449.csv")

  IPCA_historico <- read_delim("bcdata.sgs.4449.csv",
                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                           grouping_mark = "."), trim_ws = TRUE)
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

#Exemplo de uso
df_dados_hist<-
  tibble(data=as.Date("1994-07-01"), valor =400) %>%
  bind_rows(tibble(data=as.Date("1993-07-01"), valor =19800))


df_valores_constantes<- calcula_valor_constante(df_dados_hist, "2020-05-01" )



library(httr)
library(jsonlite)

# Função para obter os dados do IPCA de um determinado período
obterDadosIPCA <- function(ano_inicial, mes_inicial, ano_final, mes_final) {
  url <- paste0("https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json&dataInicial=",
                ano_inicial, "-", mes_inicial, "-01&dataFinal=", ano_final, "-", mes_final, "-01")

  resposta <- GET(url)
  dados <- content(resposta, "text")
  dados <- fromJSON(dados, flatten = TRUE)

  return(dados)
}

# Função para realizar a deflação dos valores usando o IPCA
deflacionarValores <- function(dados, coluna_valores, coluna_mes_ano) {
  # Obter dados do IPCA desde 2013 até o mês mais recente
  dados_ipca <- obterDadosIPCA(2013, 1, year(Sys.Date()), month(Sys.Date()))

  # Criar um dataframe auxiliar com os dados do IPCA
  df_ipca <- data.frame(Data = as.Date(dados_ipca$data),
                        IPCA = as.numeric(dados_ipca$valor))

  # Converter a coluna de data do dataframe original para o formato apropriado
  dados[[coluna_mes_ano]] <- as.Date(dados[[coluna_mes_ano]], format = "%Y-%m")

  # Definir a coluna de valores deflacionados
  coluna_deflacionada <- paste0(coluna_valores, "_deflacionado")

  # Realizar a deflação dos valores
  dados[[coluna_deflacionada]] <- dados[[coluna_valores]] / (1 + approx(df_ipca$Data, df_ipca$IPCA, xout = dados[[coluna_mes_ano]])$y)

  return(dados)
}

# Exemplo de uso

# Definir o nome do dataframe contendo os dados
nome_dataframe <- "contencioso_administrativo_de_primeira_instancia"

# Definir o nome da coluna com os valores a serem deflacionados
coluna_valores <- "valor_total_dos_processos"

# Definir o nome da coluna contendo a data
coluna_mes_ano <- "data"

# Obter os dados do dataframe
dados <- get(nome_dataframe)

# Deflacionar os valores
dados_deflacionados <- deflacionarValores(dados, coluna_valores, coluna_mes_ano)

# Verificar o resultado
print(dados_deflacionados)



# Instalar e carregar a biblioteca GetBCBData
install.packages("GetBCBData")
library(GetBCBData)

# Definir a série de dados do IPCA
serie_ipca <- 433

# Definir o período de início e fim desejado
ano_inicial <- 2013
mes_inicial <- 1
ano_final <- 2023
mes_final <- 6

# Obter os dados do IPCA no período desejado
dados_ipca <- gbcbd_get_series(id= serie_ipca, first_date = paste0(ano_inicial, "-", mes_inicial, "-01"),
                         last_date = paste0(ano_final, "-", mes_final, "-01"))


GetBCBData::gbcbd_get_series(
  id = serie_ipca,
  first.date = "2013-01-01",
  last.date = "2023-06-01"
)
# Carregar o dataframe contendo os dados
dados <- read.csv("caminho/do/arquivo.csv")  # Substituir pelo caminho correto do arquivo CSV

# Definir o nome da coluna com os valores a serem desinflacionados
coluna_valores <- "quantidade_processos"

# Definir o nome da coluna contendo a data
coluna_mes_ano <- "mes_ano"

# Converter a coluna de data do dataframe para o formato apropriado
dados[[coluna_mes_ano]] <- as.Date(dados[[coluna_mes_ano]], format = "%Y-%m")

# Realizar a desinflação dos valores
dados$desinflacionado <- dados[[coluna_valores]] / (1 + dados_ipca$value / 100)

# Verificar o resultado
print(dados)



serie_temporal_valor_total <- ts(contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos,
                                frequency = 12,
                                start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                          month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_total)

autoplot(decomp)


serie_temporal_valor_julgado <- ts(contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos_julgados,
                                 frequency = 12,
                                 start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                           month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_total)

autoplot(decomp)
