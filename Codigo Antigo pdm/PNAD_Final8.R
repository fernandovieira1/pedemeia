### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena e Rafaela
cat('\014')
rm(list=ls(all=TRUE)) # Comentar se não quiser limpar a memória RAM

#### 0. CONFIGURAR AMBIENTE ####
# Carregar pacotes
# Definir colunas pnad
# Criar função pnad

### 0.1 Local de trabalho ####
# local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pé-de-Meia\\BDs Pé-de-Meia'
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
carregar_pnadc <- function(anos, trimestres) {
  
  # Definir as variáveis de interesse
  variaveis <- c(
    # Localização
    'UF',      # Unidade da Federação
    'V1022',   # Situação do domicílio (rural ou urbana)
    'V1023',   # Tipo de área (Capital, RM, RIDE e UF)
    'RM_RIDE', # Região Metropolitana (r de capitais brasileiras)
    
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
    'VD4019',  # Rendimento mensal habitual (R$) (apenas 1º trimestre)
    'VD4047',  # Rendimento efetivo recebido de programas sociais, seguro-desemprego, seguro-defeso, bolsa de estudos, rendimento de caderneta de poupança e outras aplicações financeiras (apenas 1º trimestre)
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
  
  # Lista para armazenar os dados de cada ano e trimestre
  dados_completos <- list()
  
  # Iterar sobre cada ano e cada trimestre especificado
  for (ano in anos) {
    for (trimestre in trimestres) {
      # Tentar carregar os dados de cada ano e trimestre
      dados_trimestre <- tryCatch({
        get_pnadc(
          year = ano, 
          interview = trimestre, 
          defyear = ano, 
          labels = TRUE, 
          deflator = TRUE, 
          design = FALSE, 
          vars = variaveis
        )
      }, error = function(e) NULL)  # Retorna NULL caso ocorra um erro
      
      # Verificar se os dados foram carregados com sucesso e a variável existe
      if (!is.null(dados_trimestre) && "VD4047" %in% names(dados_trimestre)) {
        # Adicionar uma coluna de ano e trimestre
        dados_trimestre <- dados_trimestre %>%
          mutate(Ano = ano, Trimestre = trimestre)
        
        # Adicionar os dados do trimestre à lista
        dados_completos[[paste(ano, trimestre, sep = "_")]] <- dados_trimestre
      } else {
        warning(paste("Dados não disponíveis para o ano", ano, "e trimestre", trimestre, "ou a variável VD4047 está ausente."))
      }
    }
  }
  
  # Combinar todos os trimestres e anos em um único dataframe
  dados_final <- bind_rows(dados_completos)
  
  return(dados_final)
}


#### 1. DADOS PNADc ####
# Importar e tratar dados da pnad
# Preparar para expandir a amostra (pnad) para a população (pesos)

### 1.1 Definir ano e trimestres ####
anos <- c(2022, 2023) # 2024 não funciona
trimestres <- c(1, 2, 3, 4)

### 1.2 df prinicipal (pnad) ####
pnad <- as_tibble(carregar_pnadc(anos, trimestres))


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

pnad <- transform(pnad, V2001T = ifelse(V2005 == 'Pensionista' | # Dúvida 1: ver se mantem: impacta no Pé-de-Meia?
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
# Verificar pensionista

# Recalcular os rendimentos ajustados e agregá-los, se necessário
pnadra <- pnad %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    V2001R = sum(VD2003, na.rm = TRUE),        # Número de componentes do domicílio
    VD4016R = sum(VD4016, na.rm = TRUE),       # Total do rendimento habitual no domicílio
    VD4017R = sum(VD4017, na.rm = TRUE),       # Total do rendimento efetivo no domicílio
    VD4047R = sum(VD4019, na.rm = TRUE)        # Rendimento mensal habitual de todos os trabalhos
  ) %>%
  mutate(
    RD = VD4016R + VD4017R + VD4047R,
    RDPC = RD / V2001R # curti usou VD2003 (não considera pensionistas) - dif. irrisória
  ) %>%
  ungroup()

## *Critérios de renda ####
# Salários mínimos / ano presente (ap) ###
if (!is.vector(anos)) anos <- list(anos)

# Definir uma função para calcular o salário mínimo para qualquer ano
sal_min_ap <- function(ano) {
  case_when(
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
}

# Definir o salário mínimo para o ano mais recente disponível em 'anos'
sal_min_ap_val <- sal_min_ap(max(anos))

# Aplicar o cálculo de faixas com o valor numérico do salário mínimo
pnad <- pnad %>%
  mutate(
    RDPC_categoria = case_when(
      # Critérios de extrema pobreza e pobreza (elegibilidade para o Programa Pé-de-Meia)
      RDPC <= 105 ~ 'Extrema pobreza (até R$ 105)',
      RDPC > 105 & RDPC <= 218 ~ 'Pobreza (R$ 105,01 até R$ 218)',
      
      # Faixas adicionais baseadas no salário mínimo ajustado pelo ano
      RDPC > 218 & RDPC <= sal_min_ap_val / 4 ~ 'Acima de pobreza até 1/4 sal mín',
      RDPC > sal_min_ap_val / 4 & RDPC <= sal_min_ap_val / 2 ~ 'Mais de 1/4 até 1/2 sal mín',
      RDPC > sal_min_ap_val / 2 & RDPC <= sal_min_ap_val ~ 'Mais de 1/2 até 1 sal mín',
      RDPC > sal_min_ap_val & RDPC <= sal_min_ap_val * 2 ~ 'Mais de 1 até 2 sal mín',
      RDPC > sal_min_ap_val * 2 & RDPC <= sal_min_ap_val * 3 ~ 'Mais de 2 até 3 sal mín',
      RDPC > sal_min_ap_val * 3 & RDPC <= sal_min_ap_val * 5 ~ 'Mais de 3 até 5 sal mín',
      RDPC > sal_min_ap_val * 5 ~ 'Mais de 5 sal mín',
      TRUE ~ NA_character_
    )
  )

# Ordem das categorias
pnad <- pnad %>%
  mutate(
    RDPC_categoria = factor(RDPC_categoria, levels = c('Extrema pobreza (até R$ 105)', 
                                                       'Pobreza (R$ 105,01 até R$ 218)', 
                                                       'Acima da pobreza até 1/4 sal mín', 
                                                       'Mais de 1/4 até 1/2 sal mín', 
                                                       'Mais de 1/2 até 1 sal mín', 
                                                       'Mais de 1 até 2 sal mín', 
                                                       'Mais de 2 até 3 sal mín', 
                                                       'Mais de 3 até 5 sal mín', 
                                                       'Mais de 5 sal mín'))
  )

### 1.5 Remover colunas desnecessárias ####
# Apenas colunas necessárias para a análise
# colunas de pesos e coeficientes retiradas
pnad2 <- pnad %>%
  select(-matches("^(V103|CO|posest)")) # (2022: 376821)

### 1.6 Descritiva PNADc ####
names(pnad2)
str(pnad2)
summary(pnad2)
# FALTA Fazer descritiva

# 383409 obs. amostrais

#### 2. DADOS BRASIL ####
## Aplicação de pesos aos dados da PNADc
# População estimada, Região e UF
# Raças
# Rural ou urbana
# Pé-de-Meia

### 2.1 df pnadc ####
# Estimativa de todos os dados da pnad para o Brasil (COM pesos) (pnadc)
# A ser utilizado nos modelos de regressão e análises
pnadc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = pnad                  # Dados carregados e transformados em tibble
)
# - Para garantir que as regressões e análises sejam representativas da população, 
# usaremos o objeto pnadc em vez do dataframe pnad original
summary(pnadc)

### 2.2 População Total estimada (POP) ####
# - O objetivo aqui é verificar se a soma dos pesos é igual à população estimada
# Os pesos são calibrados para representar a população total.
# - Se esta estimativa estiver correta, as outras estimativas, mais específicas
# tenderão a refletir a realidade do programa Pé-de-Meia, pois os dados do tamanho 
# população brasileira são senso comum]
# - População estimada: 212,5 mi (2024)
# - Fonte: https://www.ibge.gov.br/apps/populacao/projecao/index.html

## *Por Região (GR) ####
pop_GR <- as.data.frame(svytotal(~factor(GR), 
                                 pnadc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pop_GR)

sum(pop_GR$total) # (2022: 214.153.641)

## *Por estado (UF) ####
pop_UF <- as.data.frame(svytotal(~factor(UF), 
                                 pnadc, na.rm = TRUE)) %>%
  mutate(Perc = (total / sum(total)) * 100)
print(pop_UF)
sum(pop_UF$total)

## *Raça ####
pop_racas <- as.data.frame(svytotal(~factor(V2010), 
                                    pnadc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pop_racas)
sum(pop_racas$total)
# todos os nao matriculados < meio salário

## *Localização - Rural ou urbana ####
pop_rur_urb <- as.data.frame(svytotal(~factor(V1022), 
                                      pnadc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pop_rur_urb)
sum(pop_rur_urb$total)

## *Gênero ####
pop_gen <- as.data.frame(svytotal(~factor(V2007), 
                                  pnadc, na.rm = TRUE)) %>%
  mutate(Perc = (total / sum(total)) * 100)
print(pop_gen)
sum(pop_gen$total)

## *Notas 2.2 ####
# - A população estimada está próxima da população oficial do IBGE
# - A distribuição por região, estado, raça, localização e gênero está coerente com a realidade

### 2.3 Rendimentos médios (POP) ####
# Dos moradores, por domicílio e per capita
# Sem recorte de idade
# RDPC: Rendimento domiciliar per capita
# Estimativa RDPC IBGE 2023: # https://biblioteca.ibge.gov.br/visualizacao/periodicos/3100/rdpc_2023.pdf
# Média estados: (2023: R$ 1.670)

## *Mensal Habitual (VD4016) ####
# VD4016AP: 4016 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4016_ap <- svyby(
  ~VD4016AP,                   # Rendimento habitual ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnadc,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4016_ap)
mean(rend_med_4016_ap$VD4016AP)/12 # (2022: R$ 2.414); (2023: R$ 2.635)

## *Mensal Efetivo (VD4017) ####
# VD4017AP: 4017 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4017_ap <- svyby(
  ~VD4017AP,                   # Rendimento Efetivo ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnadc,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4017_ap)
mean(rend_med_4017_ap$VD4017AP)/12----- # (2022: R$ 2.498); (2023: R$ 2.716)

## *Mensal Programas Sociais (VD4047) ####
# VD4047AP: 4047 deflacionado Ano presente
# Ano presente (ap), região e estado
rend_med_4047_ap <- svyby(
  ~VD4047AP,                   # Rendimento Bolsas ajustado pelo ano presente
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnadc,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_med_4047_ap)
mean(rend_med_4047_ap$VD4047AP) #  (2022: R$ 772); (2023: R$ 909)

## *Total por domicílio (RD) ####
# Ano presente (ap), região e estado
rend_medio_tot_pc_ap <- svyby(
  ~RD,                         # Rendimento domiciliar total
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnadc,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_medio_tot_pc_ap)
mean(rend_medio_tot_pc_ap$RD) #  (2022: R$ 7.467); (2023: R$ 8.023)

## *Per capita por domicílio (RDPC) ####
# Ano presente (ap), região e estado
rend_medio_dom_pc_ap <- svyby(
  ~RDPC,                         # Rendimento domiciliar total
  by = ~GR + UF,               # Agrupado por região e UF
  design = pnadc,
  FUN = svymean,
  na.rm = TRUE
)
print(rend_medio_dom_pc_ap)
mean(rend_medio_dom_pc_ap$RDPC) # (2022: R$ 2.280); (2023: R$ 2.497)

## *Notas 2.3 ####
# - Os valores de rendimento domiciliar per capita e total estão muito acima da estimativa do IBGE para 2023
# - Investigar os motivos para a discrepância
# - Definir qual utilizar

### 2.4 Público Ensino Médio ####
# em: ensino médio

# *Sem pesos (ems) ####
# Retirei o filtro de excluir famílias unipessoais
ems <- pnad %>%
  filter(
    (V2009 >= 14 & V2009 <= 24),                 # Idade entre 14 e 24 anos
    #V3002 == 'Sim',                               # Frequentam a escola
    V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
                  'Educação de jovens e adultos (EJA) do ensino médio')
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações
summary(ems)

# *Com pesos (emc) ####
# Retirei o filtro de excluir famílias unipessoais
emc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = pnad %>%
    filter(
      (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
      #V3002 == 'Sim',                                # Frequentam a escola
      V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
                    'Educação de jovens e adultos (EJA) do ensino médio')
    ) %>%
    mutate(contagem = 1)                  # Dados carregados e transformados em tibble
) 
summary(emc) 

## ****Por Região (GR) ####
emc_gr <- as.data.frame(svytotal(~factor(GR), 
                                 emc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(emc_gr)
sum(emc_gr$total)

## ****Por Estado (UF) ####
emc_uf <- as.data.frame(svytotal(~factor(UF), 
                                 emc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(emc_uf)

sum(emc_uf$total)

## *Notas 2.4 ####
# - Curti: Público Potencial (10KK-16KK?)
# - Estimativa aqui: 8.3 mi
# - Segundo a Fiocruz, em 2023 a estimativa era de que havia 7.7 mi de 
# estudantes de ensino médio no Brasil. Fonte: https://www.epsjv.fiocruz.br/noticias/reportagem/censo-escolar-revela-queda-de-150-mil-matriculas-no-ensino-medio-em-2023#:~:text=Em%202023%2C%20foram%20registradas%207,aprova%C3%A7%C3%A3o%20no%20per%C3%ADodo%20da%20pandemia%E2%80%9D.
# - O valor encontrado aqui está próximo deste número.

### 2.5 Público Pé-de-Meia ####
# Critérios e dfs necessários para a análise
#    * MEC: https://www.gov.br/mec/pt-br/pe-de-meia/publico
# pdm = Pé-de-Meia

# *Sem pesos (pdms) ####
# pdms <- pnad %>%
#   filter(
#     (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
#    # V3002 == 'Sim',                                # Frequentam a escola
#    # V3002A == 'Rede pública',                      # Rede pública de ensino
#     V3003A %in% c('Regular do ensino médio',      # Estudantes do ensino médio regular
#                   'Educação de jovens e adultos (EJA) do ensino médio'),
#     (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),       # Rendimento domiciliar per capita até metade do salário mínimo, ou
#     V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
#     VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
#   ) %>%
#   mutate(contagem = 1) # Variável para contar as observações
# summary(pdms) # 396 obs.

pdms <- pnad %>%
  mutate(sal_min_atual = sal_min_ap(Ano)) %>% # Adicionar coluna com salário mínimo do ano
  filter(
    V2009 >= 14 & V2009 <= 24 &              # Idade entre 14 e 24 anos
      V3002 == "Sim" &                       # Frequenta escola
      V3003A == "Regular do ensino médio" &  # Ensino médio regular
      V3002A == "Rede pública" &             # Escola pública
      (RDPC <= sal_min_atual / 2 |           # Renda per capita até ½ salário mínimo
         V5001A == "Sim" | V5002A == "Sim" | V5003A == "Sim") & # Benefício social
      VD2004 != "Unipessoal"                 # Domicílios não unipessoais
  ) %>%
  mutate(contagem = 1) # Variável para contar as observações
summary(pdms) # 4732 obs.

# pdms <- pnad %>%
#   filter(
#     (V2009 >= 14 & V2009 <= 24),                  # Idade entre 14 e 24 anos
#     (RDPC <= sal_min_ap / 2 | !is.na(RDPC)),      # Rendimento domiciliar per capita até metade do salário mínimo, ou
#     V5001A == 'Sim' | V5002A == 'Sim' | V5003A == 'Sim', # Recebem BPC, Bolsa Família ou outro programa social
#     VD2004 != 'Unipessoal'                         # Domicílios que não são unipessoais
#   ) %>%
#   mutate(contagem = 1) # Variável para contar as observações
# summary(pdms) # 4037 obs.

# *Com pesos (pdmc) ####
pdmc <- svydesign(
  ids = ~1,                    # IDs sem clusterização
  weights = ~V1032,            # Pesos calibrados da PNAD
  data = pdms                  # Usar o dataframe filtrado e ajustado
)
summary(pdmc)

## ****Por Região (GR) ####
pdmc_gr <- as.data.frame(svytotal(~factor(GR), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_gr)
sum(pdmc_gr$total)

## ****Por Estado (UF) ####
pdmc_uf <- as.data.frame(svytotal(~factor(UF), 
                                  pdmc, na.rm = TRUE))%>%
  mutate(Perc = (total / sum(total)) * 100)
print(pdmc_uf)
sum(pdmc_uf$total)

## *Notas 2.5 ####
# - ~2.1 mi de alunos elegíveis para o Pé-de-Meia
# - Valores coerentes com a realidade
# - Fontes:
#    * O Globo (~2 mi em 2023): https://oglobo.globo.com/economia/educacao/programa-pe-de-meia-para-estudantes-de-ensino-medio-de-familias-pobres-deve-custar-71-bilhao-em-2024-25436668
#    * Ag. Brasil (~2.7 mi em 2024): https://agenciabrasil.ebc.com.br/educacao/noticia/2024-10/pe-de-meia-pagamento-estudantes-comeca-nesta-segunda-feira#:~:text=O%20governo%20federal%20calcula%20que,ser%20sacado%20em%20qualquer%20momento

### 2.6 Custos Pé-de-Meia ####
# Com base nos critério so programa e qtde. de beneficiários
## Estimativa custo anual por aluno: 
# R$ 3.200 ano por aluno = 
# R$ 1.800 (frequência) + 
# R$   200 (matrícula) + 
# R$ 1.000 (conclusão) + 
# R$  200 (Enem)
# Fonte: https://www.gov.br/mec/pt-br/pe-de-meia/como-funciona

## *Custos estimados pdm ano ####
sum(pdmc_uf$total)*3200

## *Notas 2.6 ####
# - Estimativa de custo anual do programa Pé-de-Meia aqui: ~R$ 6.8 bi
# - Valores coerentes com a realidade
#    * Secom.gov.br (R$ 6 bi 2023): https://www.gov.br/secom/pt-br/assuntos/noticias/2024/10/pe-de-meia-pagamento-da-parcela-de-outubro-comeca-na-proxima-segunda-feira-28
#    * EBC.gov.br (~R$ 8 bi 2023): https://agenciagov.ebc.com.br/noticias/202408/pe-de-meia-expansao-tera-mais-de-1-milhao-de-beneficiados

#### 3. MODELOS ECONOMÉTRICOS PRELIMINARES ####
# Exemplo para um modelo logit
# logit_model <- svyglm(
#   abandono ~ idade + escolaridade_pai + renda,
#   design = pnadc,
#   family = quasibinomial()  
# )
# 
# summary(logit_model)

### OBSERVAÇÕES, DÚVIDAS E PENDÊNCIAS ####
## PNADc
# - Observar se Escolaridade dos pais, sexo, raça, Espécie da unidade doméstica, se trabalha e outros
# aspectos interferem na evasão escolar
# - Falta definir as variáveis de evasão escolar
# - Adicionar dummies Curti (fazer: p/ modelo painel)
# - Adaptar código pra trimestral
# PANIEL BALANCEADO ATRITO

## CADÚNICO
# Definir onde cruzar com cadunico
# ABANDONO: 1 e 2 tri, 2 e 3, 3 e 4, 4 e 5 - merge sexo + Estimar probit + socioeconomicas
# EVASAO: 1 e 5 tri - merge sexo da amostra que ingressou no 1 tri + Estimar probit + socioeconomicas
# Público alvo e fora
