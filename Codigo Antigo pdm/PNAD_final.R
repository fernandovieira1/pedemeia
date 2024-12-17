### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE E DF ####
## 0.1 Local de trabalho ####
# (!) MUDAR (se for o caso)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
setwd(local)

## 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

## 0.3 Carregar df pnad ####
# De cada trimestre
# Definir ano e trimestres desejados
ano <- 2023
trimestres <- c(1, 2, 3, 4)  # Escolha os trim que deseja, ex: c(1, 2) para apenas t1 e t2

## Variáveis relacionadas à família
variaveis <- c(
  # Localização
  'UF',      # Unidade da Federação
  'V1023',   # Localização geográfica (rural ou urbana)
  # 'V1004',   # Estrato geográfico do domicílio
  'RM_RIDE', # Região Metropolitana
  # 'GR',      # Grande Região (Norte, Nordeste, Sudeste, Sul, Centro-Oeste)
  
  # Infraestrutura e Serviços do domicílio
  # 'S01001',  # Tipo de domicílios (casas, aptos, cortiços, etc.)
  # 'S01002',  # Material de Construção
  # 'S01020',  # Revestimento das casas
  # 'S01003',  # Telhados e Pisos
  # 'S01006',  # Cômodos
  # 'S01010',  # Abastecimento de água
  # 'S01011',  # Banheiro exclusivo
  # 'S01012',  # Rede de água
  # 'S01014',  # Esgoto
  # 'S01016',  # Internet
  # 'S01018',  # Sistemas de aquecimento de água
  # 'S01023',  # Energia elétrica
  # 'S01024',  # Coleta de lixo
  
  # Condições do domicílio
  'V1008',	 # Número de série do domicílio
  'V2005',   # Condição do domicílio (permanente, improvisado, etc.)
  'VD2002',  # Propriedade e posse (próprio, aluguel, cedido, etc.)
  'VD2004',  # Indicador de residência unipessoal

  # Membros da família
  'V1014',	 # Número de série do morador
  'V2001',   # Tamanho da família
  # 'S01005',  # Número médio de pessoas por domicílio
  'V1022',   # Estrutura familiar (pai, mãe, monoparental)
  'V2003',   # Ordem do morador na família
  'V2007',   # Sexo do morador
  'V20081',  # Mês de nascimento do morador
  'V20082',  # Ano de nascimento do morador
  'V2009',   # Idade do morador
  'V2010',   # Cor ou raça do morador
  'VD2003',  # Posição do morador na família (Responsável = 1)
  'VD3004',  # Relação de Parentesco com o Responsável

  # Educação 
  'V3003',   # Curso atual ou série frequentada
  'V3002',   # Frequência escolar
  # 'V3004A',  # Modalidade de ensino (regular, supletivo ou outra)
  'V3007',   # Ano ou série que frequentava anteriormente
  'V3009A',  # Maior escolaridade atual do morador
  'VD3005',  # Educação da mãe
  'VD3006',  # Educação do pai
  'V3005A',  # Educação dos irmãos
  
  # Trabalho e Rendimento
  'V4001',   # Trabalhou na semana de referência?
  'V4009',   # Condição de ocupação
  'VD4002',  # (Horário de trabalho semanal)
  'VD4008',  # Tipo de vínculo empregatício
  'VD4010',  # Ramo em que trabalha
  'VD4016',  # Rendimento mensal efetivo do trabalho
  'VD4015',  # Outras fontes de rendimento
  'VD4019'   # Rendimento domiciliar per capita
)

# Lista para armazenar dfs trim
pnad_list <- list()

# Loop para carregar e armazenar os dados de cada trim
for (t in trimestres) {
  # Carregar dados para o trimestre `t`
  pnad_data <- get_pnadc(year = ano, quarter = t, vars = variaveis)
  
  # Criar o nome da variável dinamicamente (pnad_q1, pnad_q2, etc.)
  assign(paste0('pnad_q', t), pnad_data$variables)
  
  # Adicionar o df à lista
  pnad_list[[paste0('pnad_q', t)]] <- pnad_data$variables
}

# Juntar os dfs selecionados
pnad <- bind_rows(pnad_list)
gc(verbose=FALSE, reset=FALSE, full=TRUE)

## 0.4 Liberar RAM ####
rm(list = setdiff(ls(), 'pnad'))
gc()

names(pnad)

