### DADOS PNAD -- Análise exploratória
## Autores: Fernando, Helena, Rafaela, Curti e Ivy
# Código Curti: https://github.com/freitascurti/desafio-pe-de-meia/blob/main/probit
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
library(plm)

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

# Função para carregar dados da PNAD para múltiplos anos e trimestres
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

# Definir anos e trimestres para carregar
anos <- c(2022, 2023)     # Pode especificar um ou mais anos
trimestres <- c(1, 2) # Pode especificar um ou mais trimestres

# Carregar os dados
dados_pnad <- carregar_pnadc_multiplos(anos, trimestres)

table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

variaveis_interesse <- c( # Variáveis de interesse Curti
  'UPA', 'V1008', 'V1014', 'V2003', 'V2008', 'V20081', 'V20082', 'V2009',
  'V3003A', 'VD2004', 'V3002A', 'ID_DOMICILIO', 'V2001', 'VD4016',
  'VD4017', 'Trimestre', 'VD2003', 'VD4019', 'V2007', 'V3009A','VD2002', 
  'V3006','VD3005', 'V2010')

# Filtrar a base de dados
publico_alvo_filtrado <- dados_pnad %>%
  select(all_of(variaveis_interesse))

##################################### Criando variáveis##########################################################

##  RDPC
# Parte1 : Calcular a Renda Domiciliar Per Capita (RDPC)
rdpc_data <- publico_alvo_filtrado %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    V2001R = sum(VD2003, na.rm = TRUE),        # Número de componentes do domicílio
    VD4016R = sum(VD4016, na.rm = TRUE),       # Total do rendimento habitual no domicílio
    VD4017R = sum(VD4017, na.rm = TRUE),       # Total do rendimento efetivo no domicílio
    VD4047R = sum(VD4019, na.rm = TRUE)        # Rendimento mensal habitual de todos os trabalhos
  ) %>%
  mutate(
    RD = VD4016R + VD4017R + VD4047R,          # Rendimento domiciliar total
    RDPC = RD / V2001R                         # Renda domiciliar per capita
  ) %>%
  ungroup()


# Parte 2: Combinar RDPC com o DataFrame original
publico_alvo_filtrado <- publico_alvo_filtrado %>%
  left_join(rdpc_data, by = "ID_DOMICILIO")

# Parte 3: Adicionar as Dummies
publico_alvo_filtrado <- publico_alvo_filtrado %>%
  mutate(
    id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0),
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),  # Regular do ensino médio
    residencia_unipessoal = ifelse(VD2004 == "Unipessoal", 1, 0),             # Unidade doméstica unipessoal
    rede_publica = ifelse(V3002A == "Rede pública", 1, 0),                    # Rede pública
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0)                      # Renda domiciliar per capita menor que 706
  )


## EVASÃO
publico_alvo_filtrado <- publico_alvo_filtrado %>%
  group_by(id_individuo) %>%
  mutate(
    evasão = ifelse(
      # Condição 1: Se o indivíduo estava matriculado no 1º trimestre de 2023,
      # mas não aparece matriculado no 2º trimestre de 2023.
      ((Trimestre == "1" & V3003A == "Regular do ensino médio") & 
         !any(Trimestre == "2" & V3003A == "Regular do ensino médio")) |
        
        # Condição 2: Se o indivíduo estava matriculado no 2º trimestre de 2023,
        # mas não aparece matriculado no 3º trimestre de 2023.
        ((Trimestre == "2" & V3003A == "Regular do ensino médio") & 
           !any(Trimestre == "3" & V3003A == "Regular do ensino médio")) |
        
        # Condição 3: Se o indivíduo estava matriculado no 3º trimestre de 2023,
        # mas não aparece matriculado no 4º trimestre de 2023.
        ((Trimestre == "3" & V3003A == "Regular do ensino médio") & 
           !any(Trimestre == "4" & V3003A == "Regular do ensino médio")),
      1,  # Se alguma das condições acima for verdadeira, marcamos evasão como 1.
      0   # Caso contrário, marcamos evasão como 0.
    )
  ) %>%
  ungroup()

### (!) Aqui, se não tiver 4 semestres, quebra (!)
## Filtrando o painel para manter apenas indivíduos que aparecem nos quatro trimestres
publico_alvo_filtrado <- publico_alvo_filtrado %>%
  group_by(id_individuo) %>%
  filter(n_distinct(Trimestre) == 4) %>%  # Garante que o indivíduo está presente nos quatro trimestres
  ungroup()

## Removendo observações onde V20082 é igual a 9999
publico_alvo_painel_completo <- publico_alvo_filtrado %>%
  filter(V20082 != 9999)

## Adicionando a coluna de região
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  mutate(
    região = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_  # Para casos onde o código não corresponde a uma região
    )
  )

## Adicionando as colunas de educacao_mae e educacao_pai
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    # Identificar a mãe
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    
    # Identificar o pai
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  # Remover as colunas auxiliares e desagrupar
  select(-is_mae, -is_pai) %>%
  ungroup()

## Reorganizando as colunas para trazer as novas variáveis para o começo
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  dplyr::select(
    id_individuo, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal, 
    rede_publica, evasão, renda_per_capta_menor_706, RDPC, região, educacao_mae, educacao_pai, 
    everything()
  )

## Filtrando para beneficiar os indivíduos de interesse
beneficiários_pdm <- subset(publico_alvo_painel_completo,
                            (V2009 >= 14 & V2009 <= 24) &                # Faixa etária de 14 a 24 anos
                              V3002A == "Rede pública" &                   # Escola pública
                              V3003A == "Regular do ensino médio" &        # Ensino médio regular
                              RDPC < 706 &                                 # Renda per capita menor que 706 reais
                              VD2004 != "Unipessoal"                       # Não é residência unipessoal
)

######################################## Tamanho do problema####################################################


# Filtrar o público potencial (14-24 anos, RDPC < 706, VD2004 != "Unipessoal")

publico_pontencial <- publico_alvo_painel_completo %>%
  filter(  V2009 >= 14 & V2009 <= 24 &       # Faixa etária de 14 a 24 anos
             RDPC < 706 &                      # Renda per capita menor que 706 reais
             VD2004 != "Unipessoal" )         # Não ser residência unipessoal)

# Filtrar publico EM público

em_publico <- publico_alvo_painel_completo %>%
  filter(
    V3002A == "Rede pública" &             # Somente escolas da rede pública
      V2009 >= 14 & V2009 <= 24              # Faixa etária de 14 a 24 anos
  )


# Calcular a evasão por série, público potencial

evasao_publico_potencial <- publico_pontencial %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasão, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

# Calcular a evasão por série, EM público

evasao_em_publico <- em_publico %>%
  group_by(V3006) %>%                      # Agrupar por série
  summarise(
    evasao_total = sum(evasão, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )

# Calcular a evasão por série, beneficiários PDM

evasao_publico_alvo <- beneficiários_pdm %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasão, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasão
  )


############################################### Motivos da evasão###############################################

# Logit em painel, público potencial

modelo_logit_simples <- plm(
  evasão ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = publico_pontencial,
  model = "pooling"  # Modelo de dados agrupados (sem efeitos fixos ou aleatórios)
)

# Logit em painel, efeito fixo, público pontecial

modelo_logit_potencial <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V1022 + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = publico_pontencial,
  family = binomial(link = "logit")
)


# Logit em painel, EM público

modelo_logit_em_publico <- plm(
  evasão ~ região + educacao_mae + educacao_pai+ V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = em_publico_painel,
  model = "pooling"  # Modelo pooling (sem efeitos fixos ou aleatórios)
)

# Logit em painel, efeito fixo, EM público

modelo_logit_em_publico <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = em_publico,
  family = binomial(link = "logit")
)


# Logit em painel, público alvo

modelo_logit_publico_alvo <- plm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001,
  data = beneficiarios_pdm_painel,
  model = "pooling"  # Modelo pooling (sem efeitos fixos ou aleatórios)
)


# Logit em painel, efeito fixo, público alvo.

modelo_logit_publico_alvo <- feglm(
  evasão ~ região + educacao_mae + educacao_pai + V2001 + V2007 + V2009 + V2010 + 
    VD2004 + VD3004 + RDPC + V3001 | id_individuo,
  data = beneficiários_pdm,
  family = binomial(link = "logit")
)



########## Variáveis de interesse ###########################################################################


# região

# educacao_mae , educacao_pai 

#V1022: Situação do domicílio

#V2001: Número de pessoas no domicílio

#V2007: Sexo

#V2009: Idade do morador na data de referência

#2010: Cor ou raça

#VD2004: Espécie da unidade doméstica

#VD3004: Nível de instrução mais elevado alcançado

# RDPC : Rendimento domiciliar per capta

# V3001: Sabe ler e escrever?


################################### Número evasão################################################################

# Verificar evasões em todas as transições de trimestre
evasoes_todos_periodos <- publico_alvo_filtrado %>%
  group_by(id_individuo) %>%
  summarise(
    # Verificar matrícula no 1º trimestre e ausência no 2º trimestre
    evasao_1_para_2 = any(Trimestre == "1" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "2" & V3003A == "Regular do ensino médio"),
    
    # Verificar matrícula no 2º trimestre e ausência no 3º trimestre
    evasao_2_para_3 = any(Trimestre == "2" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "3" & V3003A == "Regular do ensino médio"),
    
    # Verificar matrícula no 3º trimestre e ausência no 4º trimestre
    evasao_3_para_4 = any(Trimestre == "3" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "4" & V3003A == "Regular do ensino médio")
  ) %>%
  # Filtrar indivíduos que tiveram evasão em qualquer um dos períodos
  filter(evasao_1_para_2 | evasao_2_para_3 | evasao_3_para_4) %>%
  ungroup()

# Verificar o número total de indivíduos que tiveram evasão em qualquer período
numero_total_de_evasoes <- nrow(evasoes_todos_periodos)
print(numero_total_de_evasoes)