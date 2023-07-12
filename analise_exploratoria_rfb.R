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


library(forecast)
library(lubridate)

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



serie_temporal_valor_total <- ts(contencioso_administrativo_de_primeira_instancia$valor_total_dos_processos,
                                frequency = 12,
                                start = c(year(min(contencioso_administrativo_de_primeira_instancia$data)),
                                          month(min(contencioso_administrativo_de_primeira_instancia$data))))

decomp <- decompose(serie_temporal_valor_total)

autoplot(decomp)
