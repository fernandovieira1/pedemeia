### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE E DF ####
## 0.1 Local de trabalho ####
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
setwd(local)

## 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

## 0.3 Definir ano e trimestres desejados ####
ano <- 2023
trimestres <- c(1, 2, 3, 4)  # Selecionar os trimestres

## 0.4 Variáveis relacionadas à família ####
variaveis <- c(
  # Localização
  'UF', 'V1023', 'RM_RIDE',
  
  # Condições do domicílio
  'V1008', 'V2005', 'VD2002', 'VD2004',
  
  # Membros da família
  'V1014', 'V2001', 'V1022', 'V2003', 'V2007', 'V20081', 'V20082', 
  'V2009', 'V2010', 'VD2003', 'VD3004',
  
  # Educação 
  'V3003', 'V3002', 'V3007', 'V3009A', 'VD3005', 'VD3006', 'V3005A',
  
  # Trabalho e Rendimento
  'V4001', 'V4009', 'VD4002', 'VD4008', 'VD4010', 'VD4016', 
  'VD4015', 'VD4019'
)

# Lista para armazenar dfs por trimestre
pnad_list <- list()

# Loop para carregar e armazenar os dados de cada trimestre
for (t in trimestres) {
  # Carregar dados para o trimestre `t`
  pnad_data <- get_pnadc(year = ano, quarter = t, vars = variaveis)
  
  # Armazenar dados do trimestre na lista
  pnad_list[[paste0('pnad_q', t)]] <- pnad_data$variables
}

# Juntar os dfs de todos os trimestres
pnad <- bind_rows(pnad_list)

# Remover linhas com valores ausentes na coluna de pesos
pnad <- pnad %>% drop_na(VD4019)

# Configurar o desenho amostral para aplicar os pesos
pnad_design <- svydesign(
  id = ~V1008,               # Unidades primárias de amostragem (UPA)
  strata = ~V1023,           # Estratos
  weights = ~VD4019,         # Pesos amostrais
  data = pnad,
  nest = TRUE
)

# Converter para convey design para medidas de desigualdade
pnad_design <- convey_prep(pnad_design)

# Verificar colunas carregadas
print(names(pnad))
