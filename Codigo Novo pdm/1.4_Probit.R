# Carregar pacotes necessários
library(survey)    
library(pscl)      
library(car)       
library(margins)   
library(ggcorrplot) 
options(survey.lonely.psu = 'adjust')  

# Limpar o ambiente
gc(); cat('\014')

################ ***************************** ################ 
#################### I. CONSTRUÇÃO DO MODELO #################### 
################ ***************************** ################ 

# 1 CRIAR DF ####
base_evasao_probit <- base_evasao_filtrada %>%
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
    educacao_mae, educacao_pai, VD3005,
    
    # Renda
    RDPC, RDPC_menor_meio_sm,
    VD4020, # Rendimento EFETIVO (R$)
    VD4013, # Horas de tabalho semanais
    
    ## 
    Ano, Trimestre 
  ) 

## Limitar a idades entre 14 e 24 anos
base_evasao_probit <- base_evasao_probit %>%
  filter(V2009 >= 14 & V2009 <= 24)

## ||| ####

# 2 CRIAR/TRANSFORMAR VARIÁVEIS ####

## a. Educação média dos pais ####
base_evasao_probit <- base_evasao_probit %>%
  mutate(educ_media_pais = (educacao_mae + educacao_pai) / 2)

## b. Educação máxima dos pais ####
base_evasao_probit <- base_evasao_probit %>%
  mutate(educ_max_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  mutate(educ_max_pais = ifelse(is.na(educacao_mae) & is.na(educacao_pai), NA, educ_max_pais))

## c. Remover valores ignorados Cor/Raça e redefinir categorias ####
base_evasao_probit <- base_evasao_probit %>%
  filter(V2010 != 'Ignorado') 

# base_evasao_probit <- base_evasao_probit %>%
#   mutate(
#     V2010 = ifelse(V2010 == 'Branca', 'Brancos', 'Não-Brancos'),
#     V2010 = as.factor(V2010)  # Garantir que seja um fator
#   )

## d. Transformar em fator regiao ####
base_evasao_probit$regiao <- as.factor(base_evasao_probit$regiao)

## e. Variável dependente como factor ####
base_evasao_probit$evasao <- as.factor(base_evasao_probit$evasao)

## f. Resumo Linhas e Colunas df ####
cat('Linhas:', nrow(base_evasao_probit), '\n')
cat('Colunas:', ncol(base_evasao_probit), '\n')

## f. Corrigir Pesos ####
base_evasao_probit <- base_evasao_probit %>%
  filter(V1028032 > 0)

## g. Converter Anos de estudo p/ numérico ####
base_evasao_probit$VD3005 <- as.numeric(base_evasao_probit$VD3005)-1

## ||| ####

# 3 AED ####
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
    method = 'circle',
    type = 'lower',
    lab = TRUE,
    title = 'Matriz de Correlação - Base Limpa'
  )

## ||| ####

# 4 DESENHO DO MODELO####
base_evasao_probit <- na.omit(base_evasao_probit)  # Remove NAs
desenho_probit <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~V1028032,
  data = base_evasao_probit
)

## ||| ####

# 5. ESTIMATIVAS DE POPULAÇÃO ####
svytotal(~evasao, desenho_probit)
svytotal(~evasao, subset(desenho_probit, Ano == 2023))

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

################ ***************************** ################ 
#################### 2. MODELO #################### 
################ ***************************** ################ 

# 1. CRIAR O MODELO PROBIT COM SVYGLM ####
probit_pdm_evasao <- svyglm(
  evasao ~ V2010 + V2007 + V1022 + VD2004 + V3002A + VD4013 +
    educ_max_pais + V2009 + V2001 + RDPC,
  
  design = desenho_probit,
  family = binomial(link = 'probit')
)

# 2. RESUMO DO MODELO ####
summary(probit_pdm_evasao)

# 3. VIF ####
vif_values <- vif(probit_pdm_evasao)
print('VIF dos coeficientes do modelo:')
print(vif_values)

# 4. EFEITOS MARGINAIS####

# 4.1 Obter predições na escala linear ####
linear_preds <- predict(probit_pdm_evasao, type = 'link')

# 4.2 MATRIZ DO MODELO ####
X <- model.matrix(probit_pdm_evasao)

# 4.3 DENSIDADE ####
densities <- dnorm(linear_preds)

# 4.4 Calcular efeitos marginais ####
marginal_effects <- sweep(X, 2, coef(probit_pdm_evasao), `*`) * densities

# 4.5 Visualizar os efeitos marginais para as primeiras observações ####
print('Efeitos marginais (primeiras observações):')
print(head(marginal_effects))

# 4.6 Analisar os efeitos marginais por variável ####
print('Média dos efeitos marginais por variável:')
marginal_means <- colMeans(marginal_effects, na.rm = TRUE)
print(marginal_means)

# 5. PSEUDO R2 ####
pseudo_r2 <- pR2(probit_pdm_evasao)
print('Pseudo R² do modelo:')
print(pseudo_r2)


# 6. NOTAS ####
# - educacao_mae e RDPC e RDPC_menor_meio_sm colineares, conforme matriz de correlacao (mc)
# - VD4020 e RDPC colineares, conforme mc
# - VD4013 irrelevante sempre
# - V3002A irrelevante sempre
# - V2009 irrelevante sempre
# - educ_max_pais irrelevante sempre
# - educacao_mae irrelevante sempre
# - V1022 irrelevante sempre
# - ensino_medio e V2009 (idade) colineares, conforme mc