# Carregando bibliotecas necessárias
library(shiny)
library(shinyWidgets)
library(gamlss)
library(tidyverse) # Coleção de pacotes para data science (ggplot2, tibble, dplyr, etc)
library(lubridate) # Manipulação de datas
library(caTools) # Médias móveis, dentre outras estatísticas úteis
library(plotly)
library(scales)
library(reshape2)
library(DT)

# Opções adicionais
options(scipen = 999, OutDec = ",")

# Definições adicionais
`%>%` <- magrittr::`%>%`
mincases <- 100
mindeaths <- 50

# Leitura dos dados
dados <- read_csv("https://data.brasil.io/dataset/covid19/caso_full.csv.gz")
dados <- dados %>%
  select(date, confirmed = last_available_confirmed, deaths = last_available_deaths,
         new_confirmed, new_deaths, place_type, city, uf = state) %>%
  mutate(uf_name = case_when(
    uf == "RO" ~ "Rondônia",
    uf == "AC" ~ "Acre",
    uf == "AM" ~ "Amazonas",
    uf == "RR" ~ "Roraima",
    uf == "PA" ~ "Pará",
    uf == "AP" ~ "Amapá",
    uf == "TO" ~ "Tocantins",
    uf == "MA" ~ "Maranhão",
    uf == "PI" ~ "Piauí",
    uf == "CE" ~ "Ceará",
    uf == "RN" ~ "Rio Grande do Norte",
    uf == "PB" ~ "Paraíba",
    uf == "PE" ~ "Pernambuco",
    uf == "AL" ~ "Alagoas",
    uf == "SE" ~ "Sergipe",
    uf == "BA" ~ "Bahia",
    uf == "MG" ~ "Minas Gerais",
    uf == "ES" ~ "Espírito Santo",
    uf == "RJ" ~ "Rio de Janeiro",
    uf == "SP" ~ "São Paulo",
    uf == "PR" ~ "Paraná",
    uf == "SC" ~ "Santa Catarina",
    uf == "RS" ~ "Rio Grande do Sul",
    uf == "MS" ~ "Mato Grosso do Sul",
    uf == "MT" ~ "Mato Grosso",
    uf == "GO" ~ "Goiás",
    uf == "DF" ~ "Distrito Federal"
  ))

dados_uf <- dados %>%
  filter(place_type == "state") %>%
  select(-city)

cities_list = c("Rio Branco","Macapá","Manaus","Belém","Porto Velho","Boa Vista","Palmas",
                "Maceió","Salvador","Fortaleza","São Luís","João Pessoa","Recife","Teresina","Natal","Aracaju",
                "Brasília","Goiânia","Cuiabá","Campo Grande",
                "Vitória","Belo Horizonte","Rio de Janeiro","São Paulo",
                "Curitiba","Porto Alegre","Florianópolis")
dados_capitais <- dados %>%
  filter(city %in% cities_list)

hoje <- max(dados_uf$date)
