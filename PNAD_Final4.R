### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
# rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE E DF ####
### 0.1 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
# setwd(local)

### 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados --> cálculo dos pesos
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

### 0.3 Função carregar_pnadc ####
carregar_pnadc <- function(ano, trimestres) {
  
  # Definir as variáveis de interesse
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
    # 'V3003',   # Curso atual ou série frequentada
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
    'VD4019',   # Rendimento domiciliar per capita
    
    ## Pesos
    'V1027', 
    'V1028',
    'V1029',
    'V1033',   
  )
  
  # Loop para carregar e armazenar os dados de cada trim
  for (t in trimestres) {
    # Carregar dados para o trimestre `t`
    pnad_data <- get_pnadc(year = ano, quarter = t, vars = variaveis)
    
    # Criar o nome da variável dinamicamente (pnad_q1, pnad_q2, etc.)
    assign(paste0('pnad_q', t), pnad_data$variables)
    
    # Adicionar o df à lista
    pnad_list[[paste0('pnad_q', t)]] <- pnad_data$variables
  }
}

### 0.4 Definir ano e trimestres ####
ano <- 2023
trimestres <- c(1, 2, 3, 4)  # Escolha os trim que deseja, ex: c(1, 2) para apenas t1 e t2

### 0.5 df prinicipal (pnad) ####
pnad <- as_tibble(carregar_pnadc(ano))

#### 1. ANÁLISE EXPLORATÓRIA (AED) ####
### 1.1 Sobre o df (pnad) ###
head(pnad)
str(pnad)
names(pnad)

### 1.2 Notas metodológicas colunas PNADc (IBGE) ####
## - [CHAVE]
# Descrição: Chaves PNADc (domicílio e pessoas)
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Chaves_PNADC.pdf

## - Variáveis derivadas
# Descrição: Códigos dos factors (categorias) das colunas PNADc 
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Definicao_variaveis_derivadas_PNADC_20200211.pdf

## - Deflatores
# Descrição: Deflatores para as variáveis de renda. 
#    * Habitual: rendas ordinárias: salário, alugueis etc.
#    * Efetivo: rendas extraordinárias: 13º salário, férias etc.
# Fontes: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#         https://www.ibge.gov.br/estatisticas/sociais/trabalho/2511-np-pnad-continua/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html?edicao=38405&t=downloads
#    * Trimestral >>> Microdados >>> Documentacao >>> Deflatores.zip

## - Variáveis PNADc 
# Descrição: Códigos das variáveis PNADc e trimestres e ano em que foram utilizadas
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#    * Trimestral >>> Microdados >>> Documentacao >> Variaveis_PNADC_Trimestral.xls

### 1.3 Colunas pnad (df local) ####
## Localização  
# UF: Unidade da Federação
# RM_RIDE: Região Metropolitana / Região Administrativa de Interesse
# UPA: Unidade Primária de Amostragem
# Estrato: Estrato de Amostragem
# V1022: Tipo de situação da região (rural ou urbana)
# V1023: Tipo de área (rural ou urbana)

## Infraestrutura e Serviços do domicílio  
# ID_DOMICILIO: Identificador único do domicílio
# V1008: Número de seleção do domicílio

## Condições do domicílio  
# V1030: Informação adicional sobre o domicílio (campo específico da pesquisa)
# V1031: Informação adicional sobre o domicílio (campo específico da pesquisa)
# V1032: Informação adicional sobre o domicílio (campo específico da pesquisa)
# V1034: Informação adicional sobre o domicílio (campo específico da pesquisa)
# VD2002: Condição no domicílio (próprio, alugado, etc.)
# VD2003: Número de componentes no domicílio
# VD2004: Espécie da unidade doméstica

## Membros da família  
# V1014: Número de série do morador
# V2001: Número de pessoas no domicílio
# V2003: Número de ordem do morador no domicílio
# V2005: Condição no domicílio (chefe, cônjuge, etc.)
# V2007: Sexo do morador
# V20081: Mês de nascimento
# V20082: Ano de nascimento
# V2009: Idade
# V2010: Cor ou raça
# VD3004: Parentesco com o chefe do domicílio

## Educação  
# V3002: Frequenta escola ou creche
# V3005A: Modalidade do ensino (fundamental, médio, etc.)
# V3007: Ano ou série que frequenta
# V3009A: Maior grau de instrução alcançado
# VD3005: Escolaridade da mãe
# VD3006: Escolaridade do pai

## Trabalho e Rendimento  
# V4001: Trabalhou na semana de referência
# V4009: Condição de ocupação
# VD4002: Horas trabalhadas na semana
# VD4008: Tipo de vínculo empregatício
# VD4010: Ramo de atividade
# VD4015: Recebimento de outras fontes de rendimento
# VD4016: Rendimento mensal efetivo do trabalho
# VD4019: Rendimento domiciliar per capita

## Outras  
# Ano: Ano de referência
# Trimestre: Trimestre de referência
# posest: Domínios de projeção geográficos
# posest_sxi: Domínios de projeção por sexo e idade
# CO1, CO1e, CO2, CO2e, CO3: Variáveis de controle específicas da PNADC
# V1028 e V1029: Pesos de calibração
# V1032001 - V1032200: Pesos replicados para cálculo de variância

### 1.4 Síntese das colunas/variáveis pnad ####

## Variáveis de análise
# Localização: 6 variáveis
# Infraestrutura e Serviços do domicílio: 2 variáveis
# Condições do domicílio: 7 variáveis
# Membros da família: 10 variáveis
# Educação: 6 variáveis
# Trabalho e Rendimento: 8 variáveis
# TOTAL: 39 variáveis

## Variáveis de peso
# - Pesos: 13 variáveis (incluindo o intervalo V1032001 - V1032200, que representa 200 
# variáveis de pesos replicados)
# TOTAL GERAL: 52 variáveis

## Tipos de peso
# - Peso Base: Expande a amostra para representar a população total e é usado 
# para estatísticas pontuais (totais, médias).
# - Peso Replicado: Permite calcular a variância dessas estimativas considerando 
# a estrutura amostral, essencial para inferência estatística (erro padrão, 
# intervalos de confiança).

## Resumo geral do df (pnad)
# - Variáveis selecionadas para análise: 39 (incluindo variáveis de localização, infraestrutura, condições do domicílio, membros da família, educação, e trabalho e rendimento)
# - Variáveis de pesos: 13, sendo:
# -  2 variáveis principais de peso (peso com calibração e peso sem calibração)
# - 200 variáveis de pesos replicados (do intervalo V1032001 até V1032200)
# TOTAL DATAFRAME: 248

### 1.5 Aplicação dos pesos no df (pnad) ####
# Peso base
pnad <- svydesign(
  id = ~UPA,
  strata = ~Estrato,
  weights = ~V1029,
  data = pnad,
  nest = TRUE
)

# Estimativa de total populacional, por exemplo, da variável "V2001" (Número de pessoas no domicílio)
pop_total <- svytotal(~V2001, pnad)
summary(pnad)