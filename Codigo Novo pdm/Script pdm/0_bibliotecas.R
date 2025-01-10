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