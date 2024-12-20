### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE E DF ####
## 0.1 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
# setwd(local)

## 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

## 0.3 Função carregar_pnadc ####
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
  
  # Lista para armazenar os dados de cada trimestre
  dados_trimestres <- map(trimestres, ~{
    get_pnadc(year = ano, quarter = .x, vars = variaveis)$variables
  })
  
  # Combinar todos os trimestres em um único dataframe
  dados_completos <- bind_rows(dados_trimestres)
  
  return(dados_completos)
}

## 0.4 Definir ano e trimestres ####
ano <- 2023
trimestres <- c(1)

### 0.5 df prinicipal (pnad) ####
pnad <- as_tibble(carregar_pnadc(ano, trimestres))

# Exibir as primeiras linhas do dataframe resultante
head(pnad_dados)
names(pnad_dados)

# Localização
# UF: Unidade da Federação
# RM_RIDE: Região Metropolitana ou Região Integrada de Desenvolvimento
# Estrato: Estrato geográfico de amostragem
# V1023: Localização geográfica (rural ou urbana)

# Infraestrutura e Serviços do Domicílio
# UPA: Unidade Primária de Amostragem (usada para estratificação)
# V1027: Indicador de domicílio particular ou coletivo
# V1029: Presença de dependências na moradia
# V1033: Condição de acesso ao esgotamento sanitário

# Condições do Domicílio
# V1008: Número de série do domicílio
# V2005: Condição do domicílio (permanente, improvisado, etc.)
# VD2002: Propriedade e posse do domicílio (próprio, aluguel, cedido, etc.)
# VD2004: Indicador de residência unipessoal

# Membros da Família
# V1014: Número de série do morador no domicílio
# V2001: Tamanho da família (número de pessoas no domicílio)
# V2003: Ordem do morador na família
# V2007: Sexo do morador
# V20081: Mês de nascimento do morador
# V20082: Ano de nascimento do morador
# V2009: Idade do morador
# V2010: Cor ou raça do morador
# VD2003: Posição do morador na família (Responsável = 1)
# VD3004: Relação de parentesco com o responsável

# Educação
# V3002: Frequência escolar do morador
# V3003: Curso atual ou série que o morador frequenta
# V3005A: Escolaridade dos irmãos
# V3007: Ano ou série escolar mais avançada que o morador frequentou
# V3009A: Maior nível de escolaridade atual do morador
# VD3005: Escolaridade da mãe
# VD3006: Escolaridade do pai

# Trabalho e Rendimento
# V4001: Indicador se trabalhou na semana de referência
# V4009: Condição de ocupação do morador
# VD4002: Horário semanal de trabalho
# VD4008: Tipo de vínculo empregatício
# VD4010: Ramo ou setor em que trabalha
# VD4015: Outras fontes de rendimento
# VD4016: Rendimento mensal efetivo do trabalho
# VD4019: Rendimento domiciliar per capita

# Outras
# Ano: Ano da pesquisa
# Trimestre: Trimestre da pesquisa
# posest: Posição do domicílio no estrato
# posest_sxi: Subposição do domicílio no estrato
# V1028: Tipo de condição de moradia (proprietário, inquilino, etc.)
# V1028001 até V1028200: Variáveis adicionais de uso específico ou regional (necessitam de definição)
# ID_DOMICILIO: Identificador único do domicílio
# Habitual: Tipo de domicílio habitual
# Efetivo: Domicílio efetivo ou temporário

