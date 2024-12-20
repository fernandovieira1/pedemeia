### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Curti

######################## 0. CONFIGURAR AMBIENTE ########################
# Notas metodológicas
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad
# Importar dados
# setwd(local)
rm(list=ls(all=TRUE)); gc(); cat('\014')

###  0. 1 Notas metodológicas ####

## *Dicionários PNADc #####
# Descrição: Dicionários de variáveis PNADc
# Fonte: - 2016: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fftp.ibge.gov.br%2FTrabalho_e_Rendimento%2FPesquisa_Nacional_por_Amostra_de_Domicilios_continua%2FAnual%2FMicrodados%2FVisita%2FVisita_1%2FDocumentacao%2Fdicionario_PNADC_microdados_2016_visita1_20220224.xls&wdOrigin=BROWSELINK
#        - 2022: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fftp.ibge.gov.br%2FTrabalho_e_Rendimento%2FPesquisa_Nacional_por_Amostra_de_Domicilios_continua%2FAnual%2FMicrodados%2FVisita%2FVisita_1%2FDocumentacao%2Fdicionario_PNADC_microdados_2022_visita1_20231129.xls&wdOrigin=BROWSELINK
#        - 2023: https://1drv.ms/x/s!AqlEsL9Wt3_5ku5rpuaUeN_JjVksCw?e=cgLGME

## *Chaves PNADc ####
# Descrição: Chaves PNADc (domicílio e pessoas)
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Chaves_PNADC.pdf

## *Variáveis derivadas ####
# Descrição: Códigos dos factors (categorias) das colunas PNADc 
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Definicao_variaveis_derivadas_PNADC_20200211.pdf

## *Deflatores ####
# Descrição: Deflatores para as variáveis de renda. 
#    * Habitual: rendas ordinárias: salário, alugueis etc.
#    * Efetivo: rendas extraordinárias: 13º salário, férias etc.
# Fontes: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#         https://www.ibge.gov.br/estatisticas/sociais/trabalho/2511-np-pnad-continua/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html?edicao=38405&t=downloads
#    * Trimestral >>> Microdados >>> Documentacao >>> Deflatores.zip

# Descrição: Códigos das variáveis PNADc e trimestres e ano em que foram utilizadas
# Fonte: https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf
#    * Trimestral >>> Microdados >>> Documentacao >> Variaveis_PNADC_Trimestral.xls

## *Evasão e abandono ####
# EVASAO: 1 e 5 tri - merge sexo da amostra que ingressou no 1 tri
# ABANDONO: 1 e 2 tri, 2 e 3, 3 e 4, 4 e 5 - merge sexo

### 0.2 Local de trabalho ####
# AVISO: Verifique o caminho do arquivo e altere-o, se for o caso
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

### 0.3 Bibliotecas ####
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

## Modelos econométricos
load_install('fixest') # Estimação de modelos fixos
load_install('lme4') # Estimação de modelos mistos
load_install('plm') # Estimação de modelos de painel
load_install('glmnet') # Regularização de modelos

## Visualização de dados
load_install('gt')  # Criar Tabelas
load_install('stargazer') # Tabelas de resultados
}

### 0.4 Definir anos e trimestres ####
# AVISO: Pode especificar um ou mais (anos ou trimestres)
anos <- c(2022, 2023, 2024)     
trimestres <- c(1, 2)

ano_inicial <- min(anos)
n_anos <- length(unique(anos))

### 0.5 Dados PNADc ####
# AVISO: Verifique o nome do arquivo e altere-o, se for o caso
df <- file.path(local, 'dados_pnad_2022-2023.rds')

## *df (DF)  ####
dados_pnad <- tryCatch({
  readRDS(df)
}, error = function(e) {
  warning('Erro ao carregar o arquivo de dados.')
  NULL
})

nrow(dados_pnad)
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

## *dados_pnad (DF) ####
# Filtrar ano(s) e trimestre(s) de interesse
# Verificar se deu certo
if (!is.null(dados_pnad)) {
  dados_pnad <- as.data.frame(dados_pnad)
  
  dados_pnad <- dados_pnad %>%
    filter(Ano %in% anos & Trimestre %in% trimestres)
  
} else {
  warning('Os dados não foram carregados corretamente.')
}

nrow(dados_pnad)
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

## 0.6 Variáveis interesse ####
variaveis_interesse <- c(
  
  ## IDENTIFICAÇÃO
  'ID_DOMICILIO',  # Identificador único do domicílio (não aparece dicionário)
  'UPA',          # Unidade Primária de Amostragem (UPA)
  'V1008',        # Nr. de seleção do domicílio (1 a 14)
  'V1014',        # Painel (indicador de panel)
  'Ano',          # Ano
  'Trimestre',    # Trimestre
  
  # DOMICÍLIO
  'VD2004',  # Espécie da unidade doméstica (1: unipessoal, 2: nuclear, 3: estendida, 4: composta)
  'VD2002',   # Condição/Parentesco no domicílio (responsável, cônjuge, filho, etc.)
  
  # FAMÍLIA
  'V2001',   # Tamanho da família (nr. de pessoas no domicílio)
  'V2003',   # Ordem do morador na família
  'V2007',   # Sexo do morador
  'V2008',   # Dia de nascimento do morador
  'V20081',  # Mês de nascimento do morador
  'V20082',  # Ano de nascimento do morador
  'V2009',   # Idade do morador
  'V2010',   # Cor ou raça do morador
  'VD2003',  # Nr. de componentes/moradores
  
  # EDUCAÇÃO
  'V3002',   # Frequenta escola?
  'V3002A',  # Tipo de escola (pública, privada, etc.)
  'V3003A',  # Curso atual ou série frequentada
  'V3006',   # Ano ou série que frequentava anteriormente
  'V3009A',  # Maior escolaridade atual do morador
  'VD3005',  # Anos de estudo completos do morador
  
  # TRABALHO E RENDA
  'VD4016',  # Rendimento mensal habitual (R$)
  'VD4017',  # Rendimento mensal efetivo (R$)
  'VD4019',  # Rendimento mensal habitual (R$) (apenas 1º trimestre)
  'VD4020'  # Rendimento mensal todos os trabalhos
)

## *publico_alvo_filtrado (DF) #### 
# Filtrar as variáveis de interesse
publico_alvo_filtrado <- dados_pnad %>%
  select(all_of(variaveis_interesse))

######################## 1. BASE EVASÃO ########################

### 1.1 base_evasao (DF) ####
# Evasão compara T1 do ano A com T1 do ano A+1

## *Criar base_evasao ####
base_evasao <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1)
table(base_evasao$Trimestre)

## *Criar Coluna id_individuo ####
base_evasao <- base_evasao %>%
  mutate(
    id_individuo = paste0(UPA, '_', # Unidade Primária de Amostragem
                          V1008, '_', # Nr. de diferenciação de domicílios na mesma UPA
                          V1014, '_', # Domicílios que permanecem na amostra da PNAD
                          V2003, '_', # Nr. de ordem (de registro na pnad) do morador no domicílio
                          V2008, '_', # Dia de nascimento
                          V20081, '_', # Mês de nascimento
                          V20082) # Ano de nascimento
  )

## *Ordenar por id_individuo, ano e trimestre ####
base_evasao <- base_evasao %>%
  arrange(id_individuo, Ano, Trimestre) 

## *Critérios Pé-de-Meia ####
# V2009: Idade
# V3003A: qual curso frequenta
# VD2004: Condição de ocupação do domicílio
base_evasao <- base_evasao %>%
  mutate(
    # Critério unificado para Geral e EJA
    ensino_medio_eja_pub = ifelse(
      (V2009 >= 14 & V2009 <= 24 & 
         V3002A == 'Rede pública' & 
         V3003A == 'Regular do ensino médio') |
        (V2009 >= 19 & V2009 <= 24 & 
           V3002A == 'Rede pública' & 
           V3003A == 'Educação de jovens e adultos (EJA) do ensino médio'),
      1, 0
    )
  ) # (?) Considerar 'NA's' como 0?

## *Calcular RD (Renda Domiciliar) #####
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE), # Rendimento domiciliar total
    ) %>%
  ungroup()

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    RDPC = RD/V2001, # V2001: Qtde residentes no domicílio
  ) %>%
  ungroup()
base_evasao$RDPC <- round(base_evasao$RDPC, 0)

## *Criar dummy renda per capita < 1/2 Sal. Mín. ####
# Função salários mínimos
sal_min <- function(ano) {
  case_when(
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

# Criar a variável dummy
base_evasao <- base_evasao %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = if_else(RDPC < (sal_min(Ano)/2), 1, 0) 
  )

## *Adicionar a coluna de região ####
base_evasao <- base_evasao %>%
  mutate(
    regiao = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ))
table(base_evasao$regiao)
prop.table(table(base_evasao$regiao))
    
## *Identificar a educação da mãe e do pai ####
# V2007: Sexo
# VD2002: Condição no domicílio (01: Condição no domicílio; 02: Cônjuge ou companheiro(a); 06: Pai, mãe, padrasto ou madrasta)
# VD3005: Grau de instrução (ensino fund. com 9 anos)
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO) %>%  # Substitua ID_DOMICILIO pelo identificador do grupo familiar, se for diferente
  mutate(
    is_mae = as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()

base_evasao %>%
  select(educacao_mae, educacao_pai) %>%
  summary()

summary(base_evasao)

## *Organizar em ordem ascendente por id, ano e trimestre ####
base_evasao <- base_evasao %>% arrange(id_individuo, Ano, Trimestre)

## *Criar a dummie de evasão ####
base_evasao <- base_evasao %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  ) %>%
  # Filtrar apenas indivíduos com mais de uma entrada
  group_by(id_individuo) %>%
  filter(n() > 1) %>%
  # Ordenar os dados por Ano e Trimestre dentro de cada indivíduo
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  # Criar a dummy de evasão
  mutate(
    evasao = ifelse(
      # Condição principal: Matriculado no 1º trimestre do ano T, mas não aparece no 1º trimestre do ano T+1
      Trimestre == 1 & V3003A == 'Regular do ensino médio' &
        !(dplyr::lead(Trimestre, default = NA_integer_) == 1 &
            dplyr::lead(Ano, default = NA_integer_) == Ano + 1 &
            dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio'),
      1,  # Marca como evasão
      0   # Caso contrário, não há evasão
    )
  ) %>%
  # Remover o agrupamento
  ungroup()

summary(base_evasao$evasao)
table(base_evasao$evasao)
prop.table(round(table(base_evasao$evasao)))
# O percentual de evasão escolar pode ser visto aqui

### 1.2 df Evasão Filtrado ####
# - Filtrar os indivíduos que responderam tanto no T1 do ano T quanto no T1 do ano T+1,
# considerando indivíduos com continuidade de presença em dois anos consecutivos no mesmo 
# trimestre (1º Trimestre).
base_evasao_filtrada <- base_evasao %>%
  group_by(id_individuo) %>%
  filter(
    any(map_lgl(map2(anos[-length(anos)], anos[-1], ~ c(.x, .y)), ~ {
      any(Ano == .x[1] & Trimestre == 1) &
        any(Ano == .x[2] & Trimestre == 1)
    }))
  ) %>%
  ungroup()

## Removendo observações onde V20082 é igual a 9999
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(V20082 != 9999) # V20082: Ano de nascimento

# Reorganizando as colunas para trazer as novas variáveis para o começo
base_evasao_filtrada <- base_evasao_filtrada %>%
  select( 
    id_individuo,Ano, Trimestre, ensino_medio_eja_pub,
    VD4020, RD, RDPC, RDPC_menor_meio_sm, regiao, educacao_mae, educacao_pai, evasao,
    salario_minimo, everything()
  )

summary(base_evasao)

######################## 2. BASE ABANDONO ########################

### 2.1 df Abandono ####
base_abandono <- base_evasao %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  ) %>%
  # Filtrar apenas indivíduos com mais de uma entrada
  group_by(id_individuo) %>%
  filter(n() > 1) %>%
  # Ordenar os dados por Ano e Trimestre dentro de cada indivíduo
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  # Criar a dummy de abandono
  mutate(
    abandono = ifelse(
      # Condição 1: Entre T1 e T2 do mesmo ano
      (Trimestre == 1 & V3003A == 'Regular do ensino médio' &
         !(dplyr::lead(Trimestre, default = NA_integer_) == 2 &
             dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
        # Condição 2: Entre T2 e T3 do mesmo ano
        (Trimestre == 2 & V3003A == 'Regular do ensino médio' &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 3 &
               dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
        # Condição 3: Entre T3 e T4 do mesmo ano
        (Trimestre == 3 & V3003A == 'Regular do ensino médio' &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 4 &
               dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
        # Condição 4: Entre T4 do ano T e T1 do ano T+1
        (Trimestre == 4 & V3003A == 'Regular do ensino médio' &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 1 &
               dplyr::lead(Ano, default = NA_integer_) == Ano + 1 &
               dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')),
      1,  # Marca como abandono
      0   # Caso contrário, não há abandono
    )
  ) %>%
  # Remover o agrupamento
  ungroup()

base_abandono <- base_abandono %>%
  # Criar a coluna id_individuo
  mutate(
    id_individuo = paste0(UPA, '_', V1008, '_', V1014, '_', V2003, '_', V2008, '_', V20081, '_', V20082)
  ) %>%
  # Organizar a base pelos identificadores e trimestres
  arrange(id_individuo, Ano, Trimestre) %>%
  # Adicionar as outras colunas e variáveis necessárias
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0), # V2009: Idade
    # ensino_medio_dummie = ifelse(V3003A == 'Regular do ensino médio', 1, 0), # V3003A: qual curso frequenta
    ensino_medio_dummie = ifelse(V3003A %in% c('Regular do ensino médio', 
                                               'Educação de jovens e adultos (EJA) do ensino médio'), 1, 0), # Mudei aqui para incluir EJA
    residencia_unipessoal = ifelse(VD2004 == 'Unipessoal', 1, 0), # VD2004: Condição de ocupação do domicílio
    rede_publica = ifelse(V3002A == 'Rede pública', 1, 0), # V3002A: Rede de ensino
    
    # Calcular RDPC (Renda Domiciliar Per Capita)
    RD = sum(VD4020, na.rm = TRUE),          # Rendimento domiciliar total
    V2001R = ifelse(!is.na(V2001), V2001[1], NA),  # nr. de residentes no domicílio
    RDPC = RD / V2001R,                      # Renda domiciliar per capita
    
    # Criar a dummy para renda per capita menor que 706
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0),
    
    # Adicionar a coluna de região
    regiao = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ),
    
    # Identificar a educação da mãe e do pai
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA),
    
    # Criar a dummie de abandono: matriculado no trimestre anterior e não matriculado no trimestre atual
    abandono = ifelse(
      lag(V3002) == 'Sim' & V3002 == 'Não' & lag(Ano) == Ano,
      1,
      0
    )
  ) %>%
  # Remover as colunas auxiliares usadas para identificar pais e mães
  select(-is_mae, -is_pai)

### 2.2 df Abandono Filtrado ####
# Reorganizando as colunas para trazer as novas variáveis para o começo
base_abandono_filtrada <- base_abandono %>%
  select( 
    id_individuo,Ano, Trimestre, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal,
    rede_publica, renda_per_capta_menor_706, RDPC, regiao, educacao_mae, educacao_pai, abandono,
    salario_minimo, everything()
  )

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

summary(base_abandono_filtrada)

######################## 3. TAMANHO DO PROBLEMA ########################
### 3.1 Públicos ####
## *Potencial ####
# Público potencial (14-24 anos, RDPC < 706)
publico_potencial <- base_abandono_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24 & RDPC < salario_minimo/2
  )

## *Pública EM ####
# Público EM público (Rede pública, 14-24 anos)
em_publico <- base_abandono_filtrada %>%
  filter(
    V3002A == 'Rede pública' & V2009 >= 14 & V2009 <= 24 
  )

## *Público PDM ####
# (Rede pública, Ensino médio regular, RDPC < 706)
beneficiários_pdm <- base_abandono_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24 &          # Faixa etária de 14 a 24 anos
      V3002A == 'Rede pública' &           # Escola pública
      V3003A == 'Regular do ensino médio' & # Ensino médio regular
      RDPC < 706                           # Renda per capita menor que 706 reais
  )

######################## 4. EVASÃO POR SÉRIE ########################
## *Público potencial ####
# Calcular a evasão por série para o público potencial
evasao_publico_potencial <- publico_potencial %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

## *Público EM ####
# Calcular a evasão por série para o público EM público
evasao_em_publico <- em_publico %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

## *Público PDM ####
# Calcular a evasão por série para os beneficiários PDM
evasao_publico_alvo <- beneficiários_pdm %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

######################## 5. MODELO DE EVASÃO ########################
## 5.1 Logit em painel, público potencial ####
modelo_logit_simples <- plm(
  evasao ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = publico_pontencial,
  model = 'pooling'  # Modelo de dados agrupados (sem efeitos fixos ou aleatórios)
)

## 5.2 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_potencial <- feglm(
  evasao ~ região + educacao_mae + educacao_pai + V1022 + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = publico_pontencial,
  family = binomial(link = 'logit')
)


## 5.3 Logit em painel, efeito fixo, público potencial ####
modelo_logit_em_publico <- plm(
  evasao ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = em_publico_painel,
  model = 'pooling'  # Modelo pooling (sem efeitos fixos ou aleatórios)
)

## 5.4 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_em_publico <- feglm(
  evasao ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = em_publico,
  family = binomial(link = 'logit')
)


## 5.5 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_publico_alvo <- plm(
  evasao ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = beneficiarios_pdm_painel,
  model = 'pooling'  # Modelo pooling (sem efeitos fixos ou aleatórios)
)

## 5.6 Logit em painel, efeito fixo, público alvo ####
modelo_logit_publico_alvo <- feglm(
  evasao ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = beneficiários_pdm,
  family = binomial(link = 'logit')
)