library(readr)
creditos_ativos <- read_delim("creditos-ativos.csv",
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                  grouping_mark = ".", encoding = "LATIN1"),
                              trim_ws = TRUE)



creditos_ativos$data<-
  str_c(creditos_ativos$ano_mes, "-01")

creditos_ativos$data <-
  as.Date(creditos_ativos$data)



