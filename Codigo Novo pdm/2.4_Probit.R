# Limpar o ambiente
gc(); cat('\014')

# Carregar pacotes
library(car) # Verificar colinearidade
library(margins) # Verificar efeitos marginais

################ 1. VARIÁVEIS DO MODELO ################ 

## ** Criar base_abandono_probit ####
base_abandono_probit <- base_abandono_filtrada %>%
  select(
    ## IDs
    UPA, id_individuo, 
    
    # Estratificação
    Estrato,  V1028032,
    
    ## Variáveis dependentes
    ensino_medio, abandono, 
    
    ## Variáveis independentes
    # Cor/Raça
    V2010,
    
    # Sexo
    V2007,
    
    # Localização do domicílio
    regiao, # do Brasil
    V1022, # do domicílio (Rural ou Urbana)
    VD2004, # Espécie da unidade doméstica (Unipessoal, Nuclear, Estendida, Composta)
    
    # Renda
    RDPC, RDPC_menor_meio_sm,
    VD4020 # Rendimento EFETIVO (R$)
  ) 

base_abandono_probit <- base_abandono_probit %>%
  filter(V2010 != "Ignorado") %>%
  mutate(V2010 = droplevels(V2010))


## **Verificar a base ####
table(base_abandono_probit$V2010,
      base_abandono_probit$V2007,
      base_abandono_probit$V1022,
      base_abandono_probit$VD2004)

#### 1.1 Desenho do modelo ####
desenho_probit <- svydesign(
  id = ~UPA,
  strata = ~Estrato,
  weights = ~V1028032,
  data = base_abandono_probit,
  nest = TRUE
)

################  2. MODELOS PROBIT PDM (EVASÃO) ################ 

#### 2.1 Probit Completo ####

## **Equação ####
probit_pdm_abandono_completo <- svyglm(
  
  # Variável dependente
  abandono ~ ensino_medio + # Se faz EM (0 ou 1)
    
    ## Variáveis independentes
    # Cor/Raça
    V2010 +
    
    # Sexo
    V2007 +
    
    # Localização do domicílio
    regiao + # do Brasil
    V1022 + # do domicílio (Rural ou Urbana)
    VD2004 + # Espécie da unidade doméstica (Unipessoal, Nuclear, Estendida, Composta)
    
    # Renda
    RDPC + RDPC_menor_meio_sm +
    VD4020, # Rendimento EFETIVO (R$)
  
  # Modelo
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## a. Resultados da Regressão ####
summary(probit_pdm_abandono_completo)

## b. Colinearidade ####
vif(probit_pdm_abandono_completo)

## c. Efeitos Marginais ####
marginais <- margins(probit_pdm_abandono_interacoes)
summary(marginais)

## d. Visualizar os Resultados ####


#### 2.2 Probit Enxuto ####

## **Equação ####
probit_pdm_abandono_enxuto <- svyglm(
  abandono ~ V2010 + V2007 + V1022 + VD2004 + RDPC_menor_meio_sm,
  design = desenho_probit,
  family = binomial(link = 'probit')
)

## a. Resultados da Regressão ####
summary(probit_pdm_abandono_enxuto)

## b. Colinearidade ####
vif(probit_pdm_abandono_enxuto)

## c. Efeitos Marginais ####
marginais_abandono_enxuto <- margins(probit_pdm_abandono_enxuto)
summary(marginais_abandono_enxuto)

## d. Visualizar os Resultados ####

## **Coeficientes do Modelo Probit ####
# Extrair coeficientes e intervalos de confiança
coeficientes <- summary(probit_pdm_abandono_enxuto)$coefficients
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
vif_df <- as.data.frame(vif(probit_pdm_abandono_enxuto)) %>%
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
marginais_df <- as.data.frame(summary(marginais_abandono_enxuto))

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