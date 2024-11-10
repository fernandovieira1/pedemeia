### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

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
    'V1022',   # Situação do domicílio (rural ou urbana)
    'V1023',   # Tipo de área (Capital, RM, RIDE e UF)
    'RM_RIDE', # Região Metropolitana (rm de capitais brasileiras)
    
    # Condições do domicílio
    'V1008',	 # Número de seleção do domicílio (1 a 14)
    'V2005',   # Condição no domicílio (responsável, cônjuge, filho, etc.)
    'VD2002',  # Condição no domicílio (responsável, cônjuge, filho, etc.)
    'VD2004',  # Espécie da unidade doméstica (1: unipessoal, 2: nuclear, 3: estendida, 4: composta)
    
    # Membros da família
    'V1014',	 # Painel
    'V2001',   # Tamanho da família (nr. pessoas domicílio)
    'V2003',   # Ordem do morador na família
    'V2007',   # Sexo do morador
    'V20081',  # Mês de nascimento do morador
    'V20082',  # Ano de nascimento do morador
    'V2009',   # Idade do morador
    'V2010',   # Cor ou raça do morador
    'VD2003',  # Nr. de componentes/moradores (1 a 30)
    
    # Educação 
    'V3002',   # Frequenta escola?
    # 'V3003',   # Curso atual ou série frequentada (NÃO RODOU 2023)
    'V3003A',  # Curso atual ou série frequentada
    'V3006',   # Ano ou série que frequentava anteriormente
    # 'V3009',  # Maior escolaridade atual do morador (NÃO RODOU 2023)
    'V3009A',  # Maior escolaridade atual do morador
    'VD3006',  # Grupos de estudo (1 a 4, 5 a 8 etc.)
    
    # Trabalho e Rendimento
    'V4001',   # Trabalhou na semana de referência?
    'V4009',   # Quantos trabalhos tinha na semana de referência?
    'VD4002',  # Se pessoas >= 14 anos estavam ocupadas ou não
    'VD4008',  # Tipo de vínculo empregatício (empregado, doméstica, militar etc.)
    'VD4009',  # Tipo de vínculo empregatício (empregado, doméstica, militar etc.)
    'VD4010',  # Ramo em que trabalha (agricultura, indústria, comércio etc.)
    'VD4011',  # Grupamentos ocupacionais (diretor, apoio adm., técnico agropecuários etc.)
    'VD4013',  # Faixa de horas de trabalho semanal
    'VD4016',  # Rendimento mensal habitual (R$)
    'VD4017',  # Rendimento mensal efetivo (R$)
    'VD4019',  # Rendimento mensal habitual (R$)
    'VD4020',  # Rendimento mensal efetivo (R$)
    'VD4047',  # Rendimento efetivo recebido de programas sociais, seguro-desemprego, seguro-defeso, bolsa de estudos, rendimento de caderneta de poupança e outras aplicações financeiras
    'V5001A',  # Recebe BPC-LOAS?
    'V5001A2', # R$ BPC-LOAS
    'V5002A',  # Recebe Bolsa Família?
    'V5002A2', # R$ Bolsa Família
    'V5003A',  # Outros programas sociais?
    'V5003A2', # R$ Outros programas sociais
    
    # ## Pesos
    'V1031',   # Peso domicílio e pessoas SEM calibração
    'V1032',   # Peso domicílio e pessoas COM calibração
    'V1034'    # Projeção da população de 1º de julho por sexo e idade
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

### 0.6 Criar/Tranformar variáveis ####
## Regiões brasileiras
pnad <-  transform(pnad, GR = as.factor(ifelse(substr(UPA, start = 1, stop = 1) == '1','Norte',
                                               ifelse(substr(UPA, start = 1, stop = 1) == '2','Nordeste',
                                                      ifelse(substr(UPA, start = 1, stop=1) == '3','Sudeste',
                                                             ifelse(substr(UPA, start = 1, stop = 1) == '4','Sul',
                                                                    ifelse(substr(UPA, start = 1, stop = 1) == '5','Centro-Oeste',NA)))))))
pnad$GR <- factor(x=pnad$GR, levels=c('Norte','Nordeste','Sudeste','Sul','Centro-Oeste'))

## *Tipos de ocupação ####
# V2001: Tamanho da família (nr. pessoas domicílio)
# V2005: Condição no domicílio (responsável, cônjuge, filho, etc.)

pnad <- transform(pnad, V2005T = ifelse(V2005 == 'Pensionista' | # Dúvida 1: ver se mantem: impacta no pé de meia?
                                          V2005 == 'Empregado(a) doméstico(a)' |
                                          V2005 == 'Parente de empregado(a) doméstico(a)',
                                        NA, 1)) # V2005T: marca as pessoas que fazem parte da família nuclear ou estendida (como "responsável", 
# "cônjuge", "filho", etc.), enquanto exclui (com NA) pessoas que não são familiares diretos, 
# como empregados domésticos e pensionistas. (T): transformado

## *Rendas atualizadas ####
# CO1: Deflator habitual ano da pesquisa (AP)
# CO1e: Deflator efetivo ano da pesquisa (AP)
# CO2: Deflator habitual ano anterior (AA)
# CO2e: Deflator efetivo ano anterior (AA)
# VD4016: Rendimento mensal habitual (R$)
# VD4017: Rendimento mensal efetivo (R$)
# VD4019: Rendimento mensal habitual (R$)
# VD4020: Rendimento mensal efetivo (R$)
# Dúvida 2: Qual a diferença entre 4016 e 4019? E entre 4017 e 4020?
# Dúvida 3: CONFIRMAR VALORES

# Ano da pesquisa (AP)
pnad <- transform(pnad, VD4016AP=ifelse(is.na(VD4016) | is.na(V2005T), 
                                        NA, VD4016*CO1)) # VD4016AP: 4016 deflacionado

pnad <- transform(pnad, VD4017AP=ifelse(is.na(VD4017) | is.na(V2005T), 
                                        NA, VD4017*CO1e)) # VD4017AP: 4017 deflacionado

pnad <- transform(pnad, VD4019AP=ifelse(is.na(VD4019) | is.na(V2005T), 
                                        NA, VD4019*CO1)) # VD4019AP: 4019 deflacionado

pnad <- transform(pnad, VD4020AP=ifelse(is.na(VD4020) | is.na(V2005T), 
                                        NA, VD4020*CO1e)) # VD4020AP: 4020 deflacionado

# Ano anterior (AA)
pnad <- transform(pnad, VD4016AA=ifelse(is.na(VD4016) | is.na(V2005T), 
                                        NA, VD4016*CO2)) # VD4016AA: 4016 deflacionado

pnad <- transform(pnad, VD4017AA=ifelse(is.na(VD4017) | is.na(V2005T), 
                                        NA, VD4017*CO2e)) # VD4017AA: 4017 deflacionado

pnad <- transform(pnad, VD4019AA=ifelse(is.na(VD4019) | is.na(V2005T), 
                                        NA, VD4019*CO2)) # VD4019AA: 4019 deflacionado

pnad <- transform(pnad, VD4020AA=ifelse(is.na(VD4020) | is.na(V2005T), 
                                        NA, VD4020*CO2e)) # VD4020AA: 4020 deflacionado

## VER DEPOIS SE VALE A PENA (Curti fez parecido)
# 1) group_by por UPA ou ID_DOMICILIO; 
# 2) somar rendas; 
# 3) calcular média por habitante do domicílio (per capita)
# 4) Comparar tudo isso em relação ao salário mínimo
# - Se sim, tirar dúvidas 2 e 3 antes

## Testes dúvida 2
head(pnad %>% select(VD4016AP, VD4016AA)) # Habitual AP e AA
tail(pnad %>% select(VD4016AP, VD4016AA)) # Efetivo AP e AA

head(pnad %>% select(VD4016AP, VD4019AA))
tail(pnad %>% select(VD4017AP, VD4020AA))

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

## - Dicionários PNADc
# Descrição: Dicionários de variáveis PNADc
# Fonte: 2016: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fftp.ibge.gov.br%2FTrabalho_e_Rendimento%2FPesquisa_Nacional_por_Amostra_de_Domicilios_continua%2FAnual%2FMicrodados%2FVisita%2FVisita_1%2FDocumentacao%2Fdicionario_PNADC_microdados_2016_visita1_20220224.xls&wdOrigin=BROWSELINK
#        2023: https://1drv.ms/x/s!AqlEsL9Wt3_5ku5rpuaUeN_JjVksCw?e=cgLGME

