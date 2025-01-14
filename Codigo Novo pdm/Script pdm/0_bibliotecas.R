### Bibliotecas ####
## Função para instalar e carregar pacotes
load_install <- function(pacote) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE)
    library(pacote, character.only = TRUE)
  }
}

{ ## Manipulação de dados 
  load_install('PNADcIBGE') # Dados PNADc
  load_install('survey') # Estratificação e Clusterização de dados - cálculo dos pesos
  load_install('convey') # Cálculo de medidas de desigualdade
  load_install('tidyverse') # Manipulação do df e Gráficos
  load_install('janitor') # Limpeza de dados
  load_install('scales')  # Formatação de gráficos
  load_install('RSQLite') # Manipulação de bases de dados SQL
  load_install('DBI') # Manipulação de grandes bases de dados
  load_install('future') # Paralelização
  load_install('furrr') # Paralelização
  load_install('arrow') # Manipulação de grandes bases de dados
  
  ## Modelos econométricos
  load_install('fixest') # Estimação de modelos fixos
  load_install('lme4') # Estimação de modelos mistos
  load_install('plm') # Estimação de modelos de painel
  load_install('glmnet') # Regularização de modelos
  
  ## Visualização de dados
  load_install('gt')  # Criar Tabelas
  load_install('stargazer') # Tabelas de resultados
  load_install('htmltools') # Exportar tabelas para HTML
}

## *Criar dummy renda per capita < 1/2 Sal. Mín. ####
# Função salários mínimos
sal_min <- function(ano) {
  case_when(
    ano == 2024 ~ 1412,
    ano == 2023 ~ 1320,
    ano == 2022 ~ 1212,
    ano == 2021 ~ 1100,
    ano == 2020 ~ 1039,
    ano == 2019 ~ 998,
    ano == 2018 ~ 954,
    ano == 2017 ~ 937,
    ano == 2016 ~ 880,
    ano == 2015 ~ 788,
    ano == 2014 ~ 724,
    ano == 2013 ~ 678,
    ano == 2012 ~ 622,
    ano == 2011 ~ 545,
    ano == 2010 ~ 510,
    TRUE ~ NA_real_
  )
}