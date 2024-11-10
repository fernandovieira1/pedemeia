### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE ####
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad

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

### 0.3 Notas metodológicas colunas PNADc (IBGE) ####
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

### 0.4 Função carregar_pnadc ####
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
    'V3002A',  # Tipo de escola (pública, privada, etc.)
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

#### 1. CONFIGURAR AMBIENTE ####
# Importar e tratar dados da pnad
# Preparar para expandir a amostra (pnad) para a população (pesos)

### 1.1 Definir ano e trimestres ####
ano <- 2023

### 1.2 df prinicipal (pnad) ####
pnad <- as_tibble(carregar_pnadc(ano))

### 1.3 Criar/Tranformar variáveis ####
## Regiões brasileiras
pnad <-  transform(pnad, GR = as.factor(ifelse(substr(UPA, start = 1, stop = 1) == '1','Norte',
                                               ifelse(substr(UPA, start = 1, stop = 1) == '2','Nordeste',
                                                      ifelse(substr(UPA, start = 1, stop=1) == '3','Sudeste',
                                                             ifelse(substr(UPA, start = 1, stop = 1) == '4','Sul',
                                                                    ifelse(substr(UPA, start = 1, stop = 1) == '5','Centro-Oeste',NA)))))))
pnad$GR <- factor(x=pnad$GR, levels=c('Norte','Nordeste','Sudeste','Sul','Centro-Oeste'))

### 1.4 Criar variáveis ####
## *Tipos de ocupação ####
# V2001: Tamanho da família (nr. pessoas domicílio)
# V2005: Condição no domicílio (responsável, cônjuge, filho, etc.)

pnad <- transform(pnad, V2001T = ifelse(V2005 == 'Pensionista' | # Dúvida 1: ver se mantem: impacta no pé de meia?
                                          V2005 == 'Empregado(a) doméstico(a)' |
                                          V2005 == 'Parente de empregado(a) doméstico(a)',
                                        NA, 1)) # V2005T: marca as pessoas que fazem parte da família nuclear ou estendida (como 'responsável', 
# 'cônjuge', 'filho', etc.), enquanto exclui (com NA) pessoas que não são familiares diretos, 
# como empregados domésticos e pensionistas. (T): transformado

## *Rendas atualizadas ####
# CO1: Deflator habitual ano da pesquisa (AP)
# CO1e: Deflator efetivo ano da pesquisa (AP)
# CO2: Deflator habitual ano anterior (AA)
# CO2e: Deflator efetivo ano anterior (AA)
# VD4016: Rendimento mensal habitual (R$)
# VD4017: Rendimento mensal efetivo (R$)
# VD4047: Rendimento efetivo recebido de programas sociais, seguro-desemprego, seguro-defeso, bolsa de estudos, rendimento de caderneta de poupança e outras aplicações financeiras

# - Dúvida 2: Qual a diferença entre 4016 e 4019? E entre 4017 e 4020?
# Resposta: nenhuma. Testei (ver PNAD_Final3.R) e são iguais, inclusive mudando CO1 e CO2. 
# Por este motivo, mantive só VD4016 e VD4017

# Dúvida 3: CONFIRMAR VALORES

# Ano da pesquisa (AP)
pnad <- transform(pnad, VD4016AP=ifelse(is.na(VD4016) | is.na(V2001T), 
                                        NA, VD4016*CO1)) # VD4016AP: 4016 deflacionado Ano presente

pnad <- transform(pnad, VD4017AP=ifelse(is.na(VD4017) | is.na(V2001T), 
                                        NA, VD4017*CO1e)) # VD4017AP: 4017 deflacionado Ano presente

pnad <- transform(pnad, VD4047AP=ifelse(is.na(VD4047) | is.na(V2001T), 
                                        NA, VD4047*CO1e)) # VD4017AP: 4047 deflacionado Ano presente

## *Rendimentos Domicílio ####
# pnadr: pnad rendimentos domiciliares
pnadr <- pnad %>% 
  group_by(ID_DOMICILIO) %>% 
  summarise(
    V2001R = sum(V2001T, na.rm = TRUE),         # Total de pessoas no domicílio consideradas para rendimento.
    VD4016R = sum(VD4016AP, na.rm = TRUE),      # Total do rendimento habitual no domicílio.
    VD4017R = sum(VD4017AP, na.rm = TRUE),      # Total do rendimento efetivo no domicílio.
    VD4047R = sum(VD4047AP, na.rm = TRUE)       # Total de rendimentos de fontes de assistência e financeiras.
  ) %>%
  mutate(
    RD = VD4016R + VD4017R + VD4047R,           # Rendimento domiciliar total.
    RDPC = RD / V2001R                          # Rendimento domiciliar per capita.
  ) %>%
  ungroup()

# Juntar pnadr ao dataframe original pnad
pnad <- pnad %>%
  left_join(pnadr, by = 'ID_DOMICILIO')

## *Ajustes Rendimento Domicílio ####
pnad <- pnad %>%
  mutate(
    # Exclui pensionistas, empregados domésticos e parentes de empregados domésticos nas variáveis de rendimento
    VD4016AP = ifelse(V2005 == 'Pensionista' | V2005 == 'Empregado(a) doméstico(a)' | V2005 == 'Parente de empregado(a) doméstico(a)', NA, VD4016AP),
    VD4017AP = ifelse(V2005 == 'Pensionista' | V2005 == 'Empregado(a) doméstico(a)' | V2005 == 'Parente de empregado(a) doméstico(a)', NA, VD4017AP),
    VD4047AP = ifelse(V2005 == 'Pensionista' | V2005 == 'Empregado(a) doméstico(a)' | V2005 == 'Parente de empregado(a) doméstico(a)', NA, VD4047AP)
  )

# Recalcular os rendimentos ajustados e agregá-los, se necessário
pnadra <- pnad %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    V2001R = sum(V2001T, na.rm = TRUE),
    VD4016R = sum(VD4016AP, na.rm = TRUE),
    VD4017R = sum(VD4017AP, na.rm = TRUE),
    VD4047R = sum(VD4047AP, na.rm = TRUE)
  ) %>%
  mutate(
    RD = VD4016R + VD4017R + VD4047R,
    RDPC = RD / V2001R
  ) %>%
  ungroup()

## *Critérios de renda ####
# Salários mínimos / ano presente (ap) ###
sal_min_ap <- case_when(
  ano == 2023 ~ 1320,
  ano == 2022 ~ 1212,
  ano == 2021 ~ 1100,
  ano == 2020 ~ 1039,
  ano == 2019 ~ 998,
  ano == 2018 ~ 954,
  ano == 2017 ~ 937,
  ano == 2016 ~ 880,
  TRUE ~ NA_real_
)


### Faixas de renda per capita ###
pnad <- pnad %>%
  mutate(
    RDPC_categoria = case_when(
      # Critérios de extrema pobreza e pobreza (elegibilidade para o Programa Pé de Meia)
      RDPC <= 105 ~ 'Extrema pobreza (até R$ 105)',
      RDPC > 105 & RDPC <= 218 ~ 'Pobreza (R$ 105,01 até R$ 218)',
      
      # Faixas adicionais baseadas no salário mínimo ajustado pelo ano
      RDPC > 218 & RDPC <= sal_min_ap / 4 ~ 'Acima de pobreza até ¼ salário mínimo',
      RDPC > sal_min_ap / 4 & RDPC <= sal_min_ap / 2 ~ 'Mais de ¼ até ½ salário mínimo',
      RDPC > sal_min_ap / 2 & RDPC <= sal_min_ap ~ 'Mais de ½ até 1 salário mínimo',
      RDPC > sal_min_ap & RDPC <= sal_min_ap * 2 ~ 'Mais de 1 até 2 salários mínimos',
      RDPC > sal_min_ap * 2 & RDPC <= sal_min_ap * 3 ~ 'Mais de 2 até 3 salários mínimos',
      RDPC > sal_min_ap * 3 & RDPC <= sal_min_ap * 5 ~ 'Mais de 3 até 5 salários mínimos',
      RDPC > sal_min_ap * 5 ~ 'Mais de 5 salários mínimos',
      TRUE ~ NA_character_
    )
  )

# Ordem das categorias
pnad <- pnad %>%
  mutate(
    RDPC_categoria = factor(RDPC_categoria, levels = c('Extrema pobreza (até R$ 105)', 'Pobreza (R$ 105,01 até R$ 218)', 'Acima de pobreza até ¼ salário mínimo', 'Mais de ¼ até ½ salário mínimo', 'Mais de ½ até 1 salário mínimo', 'Mais de 1 até 2 salários mínimos', 'Mais de 2 até 3 salários mínimos', 'Mais de 3 até 5 salários mínimos', 'Mais de 5 salários mínimos'))
  )

### 1.5 Copiar pnad (amostra) ####
# Em formato de lista, se necessário
pnadl <- as_tibble(pnad)
pnadl <- pnadc_design(data_pnadc=pnadl)

### 1.6 Remover colunas desnecessárias ####
# Apenas colunas necessárias para a análise
# colunas de pesos e coeficientes retiradas
pnad2 <- pnad %>%
  select(-matches("^(V103|CO|posest)"))

### 1.7 Descritiva PNADc ####
names(pnad2)
str(pnad2)
summary(pnad2)

# 383409 obs. amostrais

#### 2. DFs EXPANDIDOS (PESOS) ####
## Aplicação de pesos a dados da PNADc
# População estimada, Região e UF
# Raças
# Rural ou urbana
# Pé de meia

### 2.1 pnad design ####
# A ser utilizado nos modelos de regressão
pnad_design <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = pnad                  # Dados carregados e transformados em tibble
)
# - Para garantir que as regressões e análises sejam representativas da população, 
# usaremos o objeto pnad_design em vez do dataframe pnad original
summary(pnad_design)

### 2.2 População Total estimada (POP) ####

## *Por Região (GR) ####
pop_GR <- as.data.frame(svytotal(~factor(GR), pnad_design, na.rm = TRUE))
print(pop_GR)
sum(pop_GR$total) # 215.602.317 (~215.6 mi): faz sentido

## *Por estado (UF) ####
pop_UF <- as.data.frame(svytotal(~factor(UF), pnad_design, na.rm = TRUE))
print(pop_UF)
sum(pop_UF$total)

## *Raças (POP) - Divisão estimada ####
pop_racas <- as.data.frame(svytotal(~factor(V2010), pnad_design, na.rm = TRUE))
print(pop_racas)
sum(pop_racas$total)

## *Rural ou urbana (POP) ####
pop_rur_urb <- as.data.frame(svytotal(~factor(V1022), pnad_design, na.rm = TRUE))
print(pop_rur_urb)
sum(pop_rur_urb$total)

### 2.3 Pé-de-Meia (POP) ####
# pdm = Pé-de-Meia
# Dúvida 4: Ver qual faz sentido:
# https://biblioteca.ibge.gov.br/visualizacao/periodicos/3100/rdpc_2023.pdf

### 2.3.1 Rendimentos médios ####
## *Mensal Habitual (VD4016) ####
# VD4016AP: 4016 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4016_ap <- svyby(
  ~VD4016AP,                   # Rendimento habitual ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4016_ap)

## *Mensal Efetivo (VD4017) ####
# VD4017AP: 4017 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4017_ap <- svyby(
  ~VD4017AP,                   # Rendimento Efetivo ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4017_ap)

## *Mensal Programas Sociais (VD4047) ####
# VD4047AP: 4047 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4047_ap <- svyby(
  ~VD4047AP,                   # Rendimento Bolsas ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4047_ap)

## *Total por domicílio (RD) ####
# Ano presente (ap), região e estado
rend_medio_tot_pc_ap <- svyby(
  ~RD,                         # Rendimento domiciliar total
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_medio_tot_pc_ap)

## *Per capita por domicílio (RDPC) ####
# Ano presente (ap), região e estado
rend_medio_dom_pc_ap <- svyby(
  ~RDPC,                         # Rendimento domiciliar total
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnad_design,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_medio_dom_pc_ap)

### 2.3.2 Beneficiários ####
pdm <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
    V3002 == 'Sim',                                # Frequentam a escola
    V3003A == 'Regular do ensino médio',           # Estudantes do ensino médio regular
    V3002A == 'Rede pública',                      # Rede pública de ensino
    (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),       # Rendimento domiciliar per capita até metade do salário mínimo, ou
    V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
    VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações

# *Entre 14 e 24 anos que recebem algum tipo de bolsa ####
# as: assistência social
pnadas <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
    (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
    V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
    VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações


### Definindo Subconjuntos para Beneficiários

# Subconjunto beneficiários Pé-de-Meia (14 a 24 anos, critérios de renda e escolaridade)
pdm_beneficiarios <- subset(
  pnad, 
  V2009 >= 14 & V2009 <= 24 &           # Idade entre 14 e 24 anos
    V3002 == "Sim" &                      # Frequenta escola
    V3003A == "Regular do ensino médio" & # Ensino médio regular
    V3002A == "Rede pública" &            # Escola pública
    (RDPC <= sal_min_ap / 2 |             # Renda per capita até ½ salário mínimo
       V5001A == "Sim" | V5002A == "Sim" | V5003A == "Sim") & # Benefício social
    VD2004 != "Unipessoal"                # Domicílios não unipessoais
)
pdm_beneficiarios <- transform(pdm_beneficiarios, contagem = 1)

# Design amostral para o subconjunto
pdm_design <- svydesign(
  ids = ~1,
  weights = ~V1032,
  data = pdm_beneficiarios
)

# Estimativa da população de beneficiários (contagem ponderada)
contagem_beneficiados <- svyby(
  ~contagem,
  by = ~GR + UF,
  design = pdm_design,
  FUN = svytotal,
  na.rm = TRUE
)
contagem_beneficiados <- as.data.frame(contagem_beneficiados)
names(contagem_beneficiados)[1] <- "Regiao_UF"
print(contagem_beneficiados)
sum(contagem_beneficiados$contagem) # 2.125.892 (~2.1 mi)
# Faz sentido: https://agenciabrasil.ebc.com.br/educacao/noticia/2024-10/pe-de-meia-pagamento-estudantes-comeca-nesta-segunda-feira#:~:text=O%20governo%20federal%20calcula%20que,ser%20sacado%20em%20qualquer%20momento.

## Estimativa custo anual: 
# R$ 3.200 ano por aluno: R$ 1800 (frequência) + R$ 200 (matrícula) + R$ 1000 (conclusão) + R$ 200 (enem)
sum(contagem_beneficiados$contagem)*3200 
# ~R$ 6.8 bi (fez sentido: Governo estimando para 2024 R$ 7.1 bi)

## FALTA: 
# Público alvo geral em (com ou sem bf, público e privado)
# Valores em percentual
# conferir tudo
# Definir onde cruzar com cadunico
