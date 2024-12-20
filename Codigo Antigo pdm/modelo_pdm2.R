### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Curti

#### 0. CONFIGURAR AMBIENTE ####
# Notas metodológicas
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad
# Importar dados

###  0. 1 Notas metodológicas ####
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

### 0.2 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pé-de-Meia\\BDs Pé-de-Meia'
# setwd(local)
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM
gc()

### 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados - cálculo dos pesos
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos
library(fixest) # Estimação de modelos fixos
library(lme4) # Estimação de modelos mistos
library(plm) # Estimação de modelos de painel
library(janitor) # Limpeza de dados


### 0.3 Definir anos e trimestres ####
anos <- c(2022, 2023)     # Pode especificar um ou mais anos
trimestres <- c(1, 2, 3, 4) # Pode especificar um ou mais trimestres

## Função para carregar dados de múltiplos anos e trimestres
carregar_pnadc_multiplos <- function(anos, trimestres) {
  # Lista para armazenar dados de cada combinação de ano e trimestre
  dados_completos <- list()
  
  # Loop para cada ano e trimestre
  for (ano in anos) {
    for (trimestre in trimestres) {
      # Tenta carregar os dados do ano e trimestre especificados
      dados_trimestre <- tryCatch({
        # Baixa o objeto de amostra da PNADc
        pnadc_data <- get_pnadc(
          year = ano, 
          quarter = trimestre
        )
        # Extrai os dados do objeto de amostra
        as.data.frame(pnadc_data$variables)
      }, error = function(e) NULL)  # Retorna NULL em caso de erro
      
      # Verifica se o carregamento foi bem-sucedido
      if (!is.null(dados_trimestre)) {
        dados_trimestre <- dados_trimestre %>% mutate(Ano = ano, Trimestre = trimestre)
        dados_completos[[paste(ano, trimestre, sep = '_')]] <- dados_trimestre
      } else {
        warning(paste('Dados não disponíveis para o ano', ano, 'e trimestre', trimestre))
      }
    }
  }
  
  # Combina todos os dados em um único df
  dados_final <- bind_rows(dados_completos)
  
  return(dados_final)
}

### 0.4 Dados PNAD ####
## Importar dados
dados_pnad <- carregar_pnadc_multiplos(anos, trimestres)
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

## Variáveis/Colunas PNADc
variaveis_interesse <- c(
  'UPA', 'V1008', 'V1014', 'V2003', 'V2008', 'V20081', 'V20082', 'V2009',
  'V3003A', 'VD2004', 'V3002A', 'ID_DOMICILIO', 'V2001', 'VD4016',
  'VD4017', 'VD2003', 'VD4019', 'V2007', 'V3009A','VD2002', 'V3006','VD3005', 
  'V2010', 'VD4020','V3002', 'Ano', 'Trimestre')

## Definir df 
publico_alvo_filtrado <- dados_pnad %>%
  select(all_of(variaveis_interesse))

#### 1. BASE EVASÃO ####
### 1.1 df Evasão ####
# Critérios
base_evasao <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1)
table(base_evasao$Trimestre)

# Adicionar as novas colunas na base base_evasao
base_evasao <- base_evasao %>%
  # Criar a coluna id_individuo
  mutate(
    id_individuo = paste0(UPA, '_', # Unidade Primária de Amostragem
                          V1008, '_', # Nr. de diferenciação de domicílios na mesma UPA
                          V1014, '_', # Domicílios que permanecem na amostra da PNAD
                          V2003, '_', # Nr. de ordem (de registro na pnad) do morador no domicílio
                          V2008, '_', # Dia de nascimento
                          V20081, '_', # Mês de nascimento
                          V20082) # Ano de nascimento
  ) %>%
  # Organizar a base pelos identificadores, ano e trimestres
  arrange(id_individuo, Ano, Trimestre) %>%
  # Critérios Pé-de-Meia
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0), # V2009: Idade
    # ensino_medio_dummie = ifelse(V3003A == 'Regular do ensino médio', 1, 0), # V3003A: qual curso frequenta
    ensino_medio_dummie = ifelse(V3003A %in% c('Regular do ensino médio', 
                                               'Educação de jovens e adultos (EJA) do ensino médio'), 1, 0), # Mudei aqui para incluir EJA
    residencia_unipessoal = ifelse(VD2004 == 'Unipessoal', 1, 0), # VD2004: Condição de ocupação do domicílio
    rede_publica = ifelse(V3002A == 'Rede pública', 1, 0), # V3002A: Rede de ensino
    
    # Calcular RDPC (Renda Domiciliar Per Capita)
    RD = sum(VD4020, na.rm = TRUE),          # Rendimento domiciliar total --> TÁ ERRADO: tá somando tudo, não apenas o domicílio
    V2001R = ifelse(!is.na(V2001), V2001[1], NA),  # Número de residentes no domicílio 
    RDPC = RD / V2001R,                      # Renda domiciliar per capita
    
    # Criar a dummy para renda per capita menor que 706
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0), # (!) Tá errado --> mudar (sm de acordo com ano)
    
    # Adicionar a coluna de região
    região = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ),
    
    # Identificar a educação da mãe e do pai
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6))), # V2007: Sexo
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA), # VD3005: Grau de instrução (ensino fund. com 9 anos)
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA),
    
    # Criar a dummie de abandono: frequentando no trimestre anterior e não frequentando no trimestre atual
    evasão = ifelse(
      lag(V3002) == 'Sim' & V3002 == 'Não' & lag(Ano) == Ano, # V3002: Frequenta escola ou creche?
      1,
      0
    )
  ) %>%
  # Remover as colunas auxiliares usadas para identificar pais e mães
  select(-is_mae, -is_pai)

### 1.2 df Evasão Filtrado ####
# Filtrar os indivíduos que responderam tanto no T1 do ano T quanto no T1 do ano T+1
base_evasao_filtrada <- base_evasao %>%
  group_by(id_individuo) %>%
  filter(
    any(Ano == 2022 & Trimestre == 1) & any(Ano == 2023 & Trimestre == 1) |
      any(Ano == 2023 & Trimestre == 1) & any(Ano == 2024 & Trimestre == 1)
  ) %>%
  ungroup() # (!) Mudar para ficar recursiva/dinâmica, de acordo com ano e trimestre (!)

# Reorganizando as colunas para trazer as novas variáveis para o começo
base_evasao_filtrada <- base_evasao_filtrada %>%
  select( 
    id_individuo,Ano, Trimestre, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal,
    rede_publica, renda_per_capta_menor_706, RDPC, região, educacao_mae, educacao_pai, evasão,
    everything()
  )

## Removendo observações onde V20082 é igual a 9999
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(V20082 != 9999) # V20082: Ano de nascimento

#### 2. BASE ABANDONO ####
### 2.1 df Abandono ####
base_abandono <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1 | Trimestre == 2)
table(base_abandono$Trimestre)

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
    V2001R = ifelse(!is.na(V2001), V2001[1], NA),  # Número de residentes no domicílio
    RDPC = RD / V2001R,                      # Renda domiciliar per capita
    
    # Criar a dummy para renda per capita menor que 706
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0),
    
    # Adicionar a coluna de região
    região = case_when(
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
    rede_publica, renda_per_capta_menor_706, RDPC, região, educacao_mae, educacao_pai, abandono,
    everything()
  )

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

#### 3. TAMANHO DO PROBLEMA ####
# (!) Fiquei em dúvida aqui (!)
### 3.1 Públicos ####
## *Potencial ####
# Público potencial (14-24 anos, RDPC < 706)
publico_potencial <- base_abandono_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24 & RDPC < 706
  ) # (!) Errada? (!)

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

#### 4. EVASÃO POR SÉRIE ####
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
    evasao_total = sum(evasão, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

## *Público PDM ####
# Calcular a evasão por série para os beneficiários PDM
evasao_publico_alvo <- beneficiários_pdm %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasão, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

#### 5. MODELO DE EVASÃO ####
## 5.1 Logit em painel, público potencial ####
modelo_logit_simples <- plm(
  evasão ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = publico_pontencial,
  model = 'pooling'  # Modelo de dados agrupados (sem efeitos fixos ou aleatórios)
)

## 5.2 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_potencial <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V1022 + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = publico_pontencial,
  family = binomial(link = 'logit')
)


## 5.3 Logit em painel, efeito fixo, público potencial ####
modelo_logit_em_publico <- plm(
  evasão ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = em_publico_painel,
  model = 'pooling'  # Modelo pooling (sem efeitos fixos ou aleatórios)
)

## 5.4 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_em_publico <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = em_publico,
  family = binomial(link = 'logit')
)


## 5.5 Logit em painel, efeito fixo, público pontecial ####
modelo_logit_publico_alvo <- plm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = beneficiarios_pdm_painel,
  model = 'pooling'  # Modelo pooling (sem efeitos fixos ou aleatórios)
)

## 5.6 Logit em painel, efeito fixo, público alvo ####
modelo_logit_publico_alvo <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = beneficiários_pdm,
  family = binomial(link = 'logit')
)