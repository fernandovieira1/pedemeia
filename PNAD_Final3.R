### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE E DF ####
### 0.1 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
# setwd(local)

### 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
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
    'VD4019'   # Rendimento domiciliar per capita
  )
  
  # Lista para armazenar os dados de cada trimestre
  dados_trimestres <- get_pnadc(year=ano, interview=1, defyear=2023, 
              labels=TRUE, deflator=TRUE, design=FALSE, 
              vars = variaveis)
  
  # Combinar todos os trimestres em um único dataframe
  dados_completos <- bind_rows(dados_trimestres)
  
  return(dados_completos)
}

### 0.4 Definir ano e trimestres ####
ano <- 2023

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
# RM_RIDE: Região Metropolitana ou Região Integrada de Desenvolvimento
# Estrato: Estrato geográfico de amostragem
# V1023: Localização geográfica (rural ou urbana)

## Infraestrutura e Serviços do domicílio
# UPA: Unidade Primária de Amostragem (usada para estratificação) [CHAVE]
# V1027: Tipo de moradia (domicílio particular ou coletivo)
# V1032: Tipo de acesso ao saneamento (esgoto, fossa, etc.)
# posest: Posição do domicílio no estrato de amostragem
# posest_sxi: Subposição do domicílio no estrato de amostragem

## Condições do domicílio
# V1008: Número de série do domicílio [CHAVE domicílio]
# V2005: Condição do domicílio (permanente, improvisado, etc.)
# VD2002: Tipo de posse do domicílio (próprio, alugado, cedido, etc.)
# VD2004: Indicador de residência unipessoal
# ID_DOMICILIO: Identificador único do domicílio

## Membros da família
# V1014: Número de série do morador no domicílio [CHAVE]
# V2001: Tamanho da família (número total de membros)
# V1022: Estrutura familiar (pai, mãe, monoparental, etc.)
# V2003: Ordem do morador na família [CHAVE]
# V2007: Sexo do morador
# V20081: Mês de nascimento do morador
# V20082: Ano de nascimento do morador
# V2009: Idade do morador
# V2010: Cor ou raça do morador
# VD2003: Posição do morador na família (Responsável = 1)
# VD3004: Relação de parentesco com o responsável pelo domicílio

## Educação
# V3002: Frequência escolar do morador (se está matriculado ou não)
# V3005A: Escolaridade dos irmãos
# V3007: Ano ou série escolar que o morador frequentava anteriormente
# V3009A: Maior nível de escolaridade atual do morador
# VD3005: Escolaridade da mãe
# VD3006: Escolaridade do pai

## Trabalho e Rendimento
# V4001: Indicador se trabalhou na semana de referência
# V4009: Condição de ocupação (empregado, desempregado, etc.)
# VD4002: Horário semanal de trabalho
# VD4008: Tipo de vínculo empregatício
# VD4010: Ramo ou setor de atividade em que trabalha
# VD4015: Outras fontes de rendimento
# VD4016: Rendimento mensal efetivo do trabalho
# VD4019: Rendimento domiciliar per capita

## Outras
# Ano: Ano da pesquisa
# Trimestre: Trimestre da pesquisa
# V1030 até V1034: Variáveis adicionais de uso específico ou regional
# CO1, CO1e, CO2, CO2e, CO3: Variáveis com finalidades específicas na pesquisa (necessitam de definição detalhada)

