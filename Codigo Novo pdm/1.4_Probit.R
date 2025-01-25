# Limpar o ambiente
gc(); cat('\014')

# Carregar pacotes
library(car) # Verificar colinearidade
library(margins) # Verificar efeitos marginais
library(ggcorrplot) # Gráfico de correlação
options(survey.lonely.psu = 'adjust')  # Ajusta variâncias para estratos com uma única PSU


################ ***************************** ################ 
################ 1. VARIÁVEIS DO MODELO ################ 
################ ***************************** ################ 

## 1.1 CRIAR DF ####
base_evasao_probit <- base_evasao_pdm %>%
  select(
    ## IDs
    UPA, id_individuo, 
    V2009, # Idade
    V3002A, # Tipo da escola (Pública ou Privada)
    
    # Estratificação
    Estrato,  V1028032,
    
    ## Variáveis dependentes
    ensino_medio, evasao, 
    
    ## Variáveis independentes
    # Cor/Raça
    V2010,
    
    # Sexo
    V2007,
    
    # Domicílio
    regiao, # do Brasil
    V1022, # do domicílio (Rural ou Urbana)
    VD2004, # Espécie da unidade doméstica (Unipessoal, Nuclear, Estendida, Composta)
    V2001, # Tamanho do domicílio
    
    # Educação dos pais
    educacao_mae, educacao_pai,
    
    # Renda
    RDPC, RDPC_menor_meio_sm,
    VD4020, # Rendimento EFETIVO (R$)
    #VD4002, # Ocupação (p/ identificar se trabalha) --> NÃO RODA
    VD4013, # Horas de tabalho semanais
    
    ## 
    Ano, Trimestre 
  ) 

## Limitar a idades entre 14 e 24 anos
base_evasao_probit <- base_evasao_probit %>%
  filter(V2009 >= 14 & V2009 <= 24)

## ||| ####

## 1.2 CRIAR/TRANSFORMAR VARIÁVEIS ####

## a. Educação média dos pais ####
base_evasao_probit <- base_evasao_probit %>%
  mutate(educ_media_pais = (educacao_mae + educacao_pai) / 2)

## b. Educação máxima dos pais ####
base_evasao_probit <- base_evasao_probit %>%
  mutate(educ_max_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  mutate(educ_max_pais = ifelse(is.na(educacao_mae) & is.na(educacao_pai), NA, educ_max_pais))

## c. Remover valores ignorados Cor/Raça ####
base_evasao_probit <- base_evasao_probit %>%
  filter(V2010 != 'Ignorado') %>%
  mutate(V2010 = droplevels(V2010))

## d. Transformar em fator regiao ####
base_evasao_probit$regiao <- as.factor(base_evasao_probit$regiao)

## e.Variável dependente como factor ####
base_evasao_probit$evasao <- as.factor(base_evasao_probit$evasao)

## f. Resumo Linhas e Colunas df ####
cat('Linhas:', nrow(base_evasao_probit), '\n')
cat('Colunas:', ncol(base_evasao_probit), '\n')

## f. Corrigir Pesos ####
base_evasao_probit <- base_evasao_probit %>%
  filter(V1028032 > 0)

## ||| ####

## 1.3 AED ####
glimpse(base_evasao_probit)

## a. Dados numéricos ####
base_evasao_probit %>%
  select_if(is.numeric) %>%
  summary()

## b. Dados categóricos ####
base_evasao_probit %>%
  select_if(is.factor) %>%
  summary()

## c. Correlação das variáveis numéricas ####

# Tabela correlação
cor(base_evasao_probit %>%
      select_if(is.numeric) %>%
      select(-Ano, -V1028032) %>%
      select_if(~ sd(.) > 0) %>%
      na.omit(),
    use = 'pairwise.complete.obs'
)

# Gráfico correlação
base_evasao_probit %>%
  na.omit() %>%
  select_if(is.numeric) %>%
  select(-Ano, -V1028032) %>%
  select_if(~ sd(.) > 0) %>%
  cor(use = 'pairwise.complete.obs') %>%
  ggcorrplot(
    method = "circle",
    type = "lower",
    lab = TRUE,
    title = "Matriz de Correlação - Base Limpa"
  )

## ||| ####

## 1.4 DESENHO DO MODELO####

# base_evasao_probit <- base_evasao_probit %>%
#   filter(!Estrato %in% estratos_psu$Estrato)

desenho_probit <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~V1028032,
  data = base_evasao_probit,
  # nest = TRUE
)

svytotal(~evasao, desenho_probit)
svytotal(~evasao, subset(desenho_probit, Ano == 2023))


## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

################ ***************************************** ################ 
################  2. MODELOS PROBIT PDM (EVASÃO) ################ 
################ ***************************************** ################

## 2.1 PROBIT COMPLETO ####

## a. Equação ####
probit_pdm_evasao_completo <- svyglm(
  
  # Variável dependente
  evasao ~ V2009 + V3002A + ensino_medio + V2010 + V2007 + regiao + V1022 + 
    VD2004 + V2001 + educacao_mae + educacao_pai + RDPC + RDPC_menor_meio_sm + 
    VD4020 + VD4013 + Ano + Trimestre + educ_media_pais + educ_max_pais,
  
  # Modelo
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## b. Resultados da Regressão ####
summary(probit_pdm_evasao_completo)

## c. Colinearidade ####
vif(probit_pdm_evasao_completo)

## d. Efeitos Marginais ####
marginais <- margins(probit_pdm_evasao_completo)
summary(marginais)

## ||| ####

## 2.2 PROBIT ENXUTO ####

## a. Equação ####
probit_pdm_evasao_enxuto <- svyglm(
  evasao ~ V2010 + V2007 + V1022 + VD2004 + RDPC_menor_meio_sm,
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## b. Resultados da Regressão ####
summary(probit_pdm_evasao_enxuto)

## c. Colinearidade ####
vif(probit_pdm_evasao_enxuto)

## d. Efeitos Marginais ####
marginais_evasao_enxuto <- margins(probit_pdm_evasao_enxuto, design = desenho_probit)
summary(marginais_evasao_enxuto)

## ||| ####

## 2.3 PROBIT TESTES ####

## **Equação ####
probit_pdm_evasao_testes <- svyglm(
  evasao ~ V2010 + V2007 + V1022 + VD2004 + RDPC_menor_meio_sm +
    educ_media_pais + VD4013,
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## a. Resultados da Regressão ####
summary(probit_pdm_evasao_testes)

## b. Colinearidade ####
vif(probit_pdm_evasao_testes)

## c. Efeitos Marginais ####
marginais_evasao_testes <- margins(probit_pdm_evasao_testes, design = desenho_probit)
summary(marginais_evasao_enxuto)

## ||| ####

## 2.4 PROBIT FINAL ####

## a. Equação ####
probit_pdm_evasao_final <- svyglm(
  # Categóricas
  evasao ~ V2010 + V2007 + V1022 + VD2004 + V3002A + VD4013 +
  # Numéricas
    educ_max_pais + V2009 + V2001 + RDPC,
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## b. Resultados da Regressão ####
summary(probit_pdm_evasao_final)

## c. Colinearidade ####
vif(probit_pdm_evasao_final)

## d. Efeitos Marginais ####
marginais_evasao_testes <- margins(probit_pdm_evasao_final, design = desenho_probit)
summary(marginais_evasao_enxuto)

## ||| ####

## 2.5 PROBIT FERNANDO ####

## a. Equação ####
probit_pdm_evasao_fernando <- svyglm(
  # Categóricas
  evasao ~ V2010 + V2007 + VD2004 + V3002A +
        # removi: 
        # - V1022 (rural ou urbana)
        # - VD4013 (horas de trabalho semanais)
    
    # Numéricas
    educ_max_pais + V2009 + V2001 + RDPC,
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## b. Resultados da Regressão ####
summary(probit_pdm_evasao_fernando)

## c. Colinearidade ####
vif(probit_pdm_evasao_fernando)

## d. Efeitos Marginais ####
marginais_evasao_testes <- margins(probit_pdm_evasao_fernando, design = desenho_probit)
summary(marginais_evasao_enxuto)

## ||| ####

## VISUALIZAR RESULTADOS ####

## **Coeficientes do Modelo Probit ####
# Extrair coeficientes e intervalos de confiança
coeficientes <- summary(probit_pdm_evasao_fernando)$coefficients
coef_df <- as.data.frame(coeficientes) %>%
  rownames_to_column(var = 'Variável') %>%
  mutate(
    Inferior = Estimate - 1.96 * `Std. Error`,
    Superior = Estimate + 1.96 * `Std. Error`
  )

# Plotar os coeficientes
ggplot(coef_df, aes(x = reorder(Variável, Estimate), y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Inferior, ymax = Superior), width = 0.2) +
  coord_flip() +
  labs(
    title = 'Coeficientes do Modelo Probit',
    x = 'Variáveis',
    y = 'Coeficiente Estimado'
  ) +
  theme_minimal()

## **Colinearidade ####
# Extrair VIFs
vif_df <- as.data.frame(vif(probit_pdm_evasao_fernando)) %>%
  rownames_to_column(var = 'Variável')

# Gráfico de barras para VIFs
ggplot(vif_df, aes(x = reorder(Variável, GVIF), y = GVIF^(1/(2*Df)))) +
  geom_bar(stat = 'identity', fill = 'tomato', alpha = 0.8) +
  coord_flip() +
  labs(
    title = 'Fatores de Inflação da Variância (VIF) / Colinearidade',
    x = 'Variáveis',
    y = 'GVIF^(1/(2*Df))'
  ) +
  theme_minimal()

## **Efeitos Marginais ####
# Converter efeitos marginais para um dataframe
marginais_df <- as.data.frame(summary(probit_pdm_evasao_fernando))

# Criar o gráfico dos efeitos marginais
ggplot(marginais_df, aes(x = reorder(factor, AME), y = AME)) +
  geom_bar(stat = 'identity', fill = 'steelblue', alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = 'black') +
  coord_flip() +
  labs(
    title = 'Efeitos Marginais Médios (AMEs)',
    x = 'Variáveis',
    y = 'Efeito Marginal'
  ) +
  theme_minimal()

## ||| ####

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## COLUNAS ####
# UPA                : Unidade Primária de Amostragem
# id_individuo       : Identificador do indivíduo
# V2009              : Idade
# V3002A             : Tipo da escola (Pública ou Privada)
# Estrato            : Estratificação da PNADc
# V1028032           : Peso amostral
# ensino_medio       : Indicador de conclusão do ensino médio
# evasao             : Indicador de evasão
# V2010              : Cor/Raça
# V2007              : Sexo
# V2001              : Tamanho do domicílio
# regiao             : Região do Brasil
# V1022              : Localização do domicílio (Rural ou Urbana)
# VD2004             : Espécie da unidade doméstica (Unipessoal, Nuclear, Estendida, Composta)
# educacao_mae       : Anos de estudo da mãe
# educacao_pai       : Anos de estudo do pai
# RDPC               : Renda domiciliar per capita
# RDPC_menor_meio_sm : Renda domiciliar per capita menor que meio salário mínimo
# VD4020             : Rendimento EFETIVO (R$)
# VD4013             : Horas de tabalho semanais (em intervalos. P. ex: 15 a 39)
# educ_media_pais    : Média da educação dos pais
# educ_max_pais      : Máximo da educação dos pais
