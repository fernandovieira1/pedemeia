### DADOS PNAD -- Análise exploratória

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
# AVISO: Verifique abaixo o caminho do arquivo e altere-o, se for o caso

## Windows
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

## Ubuntu
# local <- '~/Insync/fernandovieira1@outlook.com/OneDrive/1. Educacao/2. Academia/3. DOUTORADO/USP - Economia Aplicada/MATERIAS/Eco II - Daniel/Desafio Eco II - Pe de Meia/BDs Pe de Meia'

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
anos <- c(2022, 2023)     
trimestres <- c(1, 2, 3, 4)

ano_inicial <- min(anos)
n_anos <- length(unique(anos))

### 0.5 Dados PNADc ####
# AVISO 1: Verifique o nome do arquivo e altere-o, se for o caso
# AVISO 2: Os dados da PNAD entre 2015 e 2024 encontram-se disponíveis para download em: https://1drv.ms/f/s!AqlEsL9Wt3_5kvASEz8uIW1-ZxyB4g
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

## 0.6 Variáveis interesse ####
variaveis_interesse <- c(
  
  ## IDENTIFICAÇÃO
  'ID_DOMICILIO',  # Identificador único do domicílio (não aparece dicionário)
  'UPA',          # Unidade Primária de Amostragem (UPA)
  'V1008',        # Nr. de seleção do domicílio (1 a 14)
  'V1014',        # Painel (indicador de panel)
  'V1016',        # Indica a entrevista (1 a 5)
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
  'V3006',   # Qual é o ano/série/semestre que frequenta?
  'V3008',   # Anteriormente frequentou escola?
  'V3013',   # Qual foi o último ano/série que concluiu com aprovação no curso frequentado anteriormente
  'V3014',   # Concluiu este curso que frequentou anteriormente?
  
  # TRABALHO E RENDA
  'VD4013',  # Faixa das horas habitualmente trabalhadas por semana em todos os trabalhos (14 anos ou mais)
  'VD4014',  # Faixa das horas efetivamente trabalhadas por semana em todos os trabalhos  (14 anos ou mais)
  'VD4016',  # Rendimento mensal habitual (R$)
  'VD4017',  # Rendimento mensal efetivo (R$)
  'VD4019',  # Rendimento mensal habitual (R$) (apenas 1º trimestre)
  'VD4020',  # Rendimento mensal todos os trabalhos
  'V4012',   # Neste trabalho, era... (excluir militares e funcionários públicos da análise de salários)
  'V4025',   # Nesse trabalho, ... era contratado(a) como empregado temporário ?
  'V403312', # Qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ?
  'V4039C',  # Quantas horas ... trabalhou efetivamente na semana de referência nesse trabalho principal?
  'V4040',   # Até o dia (último dia da semana de referência) fazia quanto tempo que estava nesse trabalho?
  'V405012', # Valor em dinheiro do rendimento mensal que recebia normalmente nesse trabalho secundário
  'V4056',   # Quantas horas ... trabalhava normalmente, por semana, nesse trabalho secundário?
  'VD4018',  # Tipo de remuneração recebida em todos os trabalhos (1 = dinheiro, 2 = beneficios ou sem rem.)
  'VD4019'   # Rendimento de todos os trabalhos (confirmar se é isso mesmo)
)

## *dados_pnad - Variáveis de interesse ####
dados_pnad <- dados_pnad %>%
  select(all_of(variaveis_interesse))

## *dados_pnad (DF) - Filtrar períodos ####
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

## *publico_alvo_filtrado (DF) #### 
# Filtrar as variáveis de interesse
publico_alvo_filtrado <- dados_pnad  # Apenas mudei o nome pelo código legado de outras versões.

nrow(publico_alvo_filtrado)
table(publico_alvo_filtrado$Ano)
table(publico_alvo_filtrado$Trimestre)

## Remover df dados_pnad 
# liberar RAM
rm(dados_pnad)

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
    ensino_medio = ifelse(
      (V2009 >= 14 & V2009 <= 24 & 
         V3002A == 'Rede pública' & 
         V3003A == 'Regular do ensino médio'),
      1, 0
    )
  ) # (?) Considerar 'NA's' como 0?
# Retirado EJA (Fazer em separado depois) - em 'modelo_pdm7.R' (27/11/2024)

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

## *Organizar em ordem ascendente por id, ano e trimestre ####
base_evasao <- base_evasao %>% 
  arrange(id_individuo, Ano, Trimestre)

## *Criar a dummy de evasão ####
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
    id_individuo,
    Ano, 
    Trimestre, 
    ensino_medio,
    VD4020, # Rendimento mensal todos os trabalhos
    RD, 
    RDPC, 
    RDPC_menor_meio_sm, 
    regiao, 
    educacao_mae, 
    educacao_pai, 
    evasao,
    salario_minimo, 
    everything()
  )

summary(base_evasao_filtrada)
str(base_evasao_filtrada)
table(base_evasao_filtrada$Ano)
table(base_evasao_filtrada$Trimestre)

table(base_evasao_filtrada$evasao)
prop.table(round(table(base_evasao_filtrada$evasao)))
# O percentual de evasão escolar (=1) pode ser visto aqui

######################## 2. BASE ABANDONO ########################

### 2.1 base_abandono (DF) ####
# Abandono compara 1 e 2 tri, 2 e 3 tri, 3 e 4 tri.
base_abandono <- publico_alvo_filtrado %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  ) 

## *Criar Coluna id_individuo ####
base_abandono <- base_abandono %>%
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
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre)

## *Critérios Pé-de-Meia ####
# V2009: Idade
# V3003A: qual curso frequenta
# VD2004: Condição de ocupação do domicílio
base_abandono <- base_abandono %>%
  mutate(
    # Critério unificado para Geral
    ensino_medio = ifelse(
      (V2009 >= 14 & V2009 <= 24 & 
         V3002A == 'Rede pública' & 
         V3003A == 'Regular do ensino médio'),
      1, 0
    )
  ) # (?) Considerar 'NA's' como 0?
# EJA retirado em modelo_pdm7 / inserir depois

## *Calcular RD (Renda Domiciliar) #####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE), # Rendimento domiciliar total
  ) %>%
  ungroup()

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    RDPC = RD/V2001, # V2001: Qtde residentes no domicílio
  ) %>%
  ungroup()
base_abandono$RDPC <- round(base_abandono$RDPC, 0)

# Criar a variável dummy sm
base_abandono <- base_abandono %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = if_else(RDPC < (sal_min(Ano)/2), 1, 0) 
  )

## *Adicionar a coluna de região ####
base_abandono <- base_abandono %>%
  mutate(
    regiao = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ))
table(base_abandono$regiao)
prop.table(table(base_abandono$regiao))

## *Identificar a educação da mãe e do pai ####
# V2007: Sexo
# VD2002: Condição no domicílio (01: Condição no domicílio; 02: Cônjuge ou companheiro(a); 06: Pai, mãe, padrasto ou madrasta)
# VD3005: Grau de instrução (ensino fund. com 9 anos)
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%  # Substitua ID_DOMICILIO pelo identificador do grupo familiar, se for diferente
  mutate(
    is_mae = as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()

## *Criar a dummy de abandono ####
base_abandono <- base_abandono %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  ) %>%
  # Filtrar apenas indivíduos com mais de uma entrada válida
  group_by(id_individuo) %>%
  filter(n_distinct(Trimestre) > 1) %>%
  # Ordenar os dados por Ano e Trimestre dentro de cada indivíduo
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  # Criar a dummy de abandono
  mutate(
    abandono = ifelse(
      # Apenas considerar trimestres 2, 3 e 4 para cálculo de abandono
      Trimestre > 1 & (
        # Condição 1: Entre T1 e T2 do mesmo ano
        (Trimestre == 2 & V3003A == 'Regular do ensino médio' &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 3 &
               dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
          # Condição 2: Entre T2 e T3 do mesmo ano
          (Trimestre == 3 & V3003A == 'Regular do ensino médio' &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 4 &
                 dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
          # Condição 3: Entre T3 e T4 do mesmo ano
          (Trimestre == 4 & V3003A == 'Regular do ensino médio' &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 1 &
                 dplyr::lead(Ano, default = NA_integer_) == Ano + 1 &
                 dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio'))
      ),
      1,  # Marca como abandono
      0   # Caso contrário, não há abandono
    )
  ) %>%
  # Remover o agrupamento
  ungroup()
# Obs.: demora o processamento aqui

## Verificar abandono por trimestre
cat('Distribuição por trimestre e abandono:\n')
base_abandono %>%
  filter(!is.na(abandono)) %>%  
  count(Trimestre, abandono) %>%
  pivot_wider(
    names_from = abandono,
    values_from = n,
    names_prefix = 'Absoluto_'
  ) %>%
  mutate(
    Absoluto_0 = replace_na(Absoluto_0, 0),  
    Absoluto_1 = replace_na(Absoluto_1, 0),
    Relativo_1 = round((Absoluto_1 / (Absoluto_0 + Absoluto_1)) * 100, 5)  
  ) %>%
  select(Trimestre, Absoluto_0, Absoluto_1, Relativo_1) %>% 
  arrange(Trimestre)

## Verificar taxa de abandono
cat('\nTaxa de abandono (4 trimestres):\n')
print(prop.table(table(base_abandono$abandono)))

table(base_abandono$Ano)
table(base_abandono$Trimestre)

### 2.2 df Abandono Filtrado ####
# Reorganizando as colunas para trazer as novas variáveis para o começo
base_abandono_filtrada <- base_abandono %>%
  select( 
    id_individuo,
    Ano, 
    Trimestre, 
    ensino_medio,
    VD4020, # Rendimento mensal todos os trabalhos
    RD, 
    RDPC, 
    RDPC_menor_meio_sm, 
    regiao, 
    educacao_mae, 
    educacao_pai, 
    abandono,
    salario_minimo, 
    everything()
  )

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

table(base_abandono_filtrada$abandono)
prop.table(round(table(base_abandono_filtrada$abandono)))
# O percentual de abandono escolar (todos os períodos) pode ser visto aqui

