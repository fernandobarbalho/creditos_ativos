library(tidyverse)
creditos_ativos <- read_delim("data/creditos-ativos.csv",
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                  grouping_mark = ".", encoding = "LATIN1"),
                              trim_ws = TRUE)


contencioso_administrativo_de_primeira_instancia <- read_delim("data/contencioso-administrativo-de-primeira-instancia.csv",
                                                               delim = ";", escape_double = FALSE, col_types = cols(`Data da publicação` = col_date(format = "%d/%m/%Y")),
                                                               locale = locale(decimal_mark = ",", grouping_mark = ".",
                                                                               encoding = "Latin1"), trim_ws = TRUE)


contencioso_administrativo_de_primeira_instancia <-  janitor::clean_names(contencioso_administrativo_de_primeira_instancia)


contencioso_administrativo_de_primeira_instancia$data<-
  str_c("01-",contencioso_administrativo_de_primeira_instancia$mes_ano)

contencioso_administrativo_de_primeira_instancia$data <-
  as.Date(contencioso_administrativo_de_primeira_instancia$data, format = "%d-%B-%y")


