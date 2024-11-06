##Baixando pacotes
#install.packages("PNADcIBGE")
#install.packages("survey")
install.packages("htmltools")
install.packages("webshot2")


#Carregando pacotes
library(PNADcIBGE)
library(dplyr)
library(stargazer)
library(haven)
library(foreign)
library(survey)
library(summarytools)
library(webshot2)
library(htmltools)
library(tidyr)
library(gtsummary)
library(glue)
library(rlang)
library(reactable)
library(ggplot2)
library(broom)

# Importando o 1º trimestre de 2024 como data frame
dados_pnadc_2024_trim1 <- get_pnadc(year = 2024, quarter = 1, design = FALSE)

# Importando o 2º trimestre de 2024 como data frame
dados_pnadc_2024_trim2 <- get_pnadc(year = 2024, quarter = 2, design = FALSE)

# Filtrando idade entre 14 e 24 para ambos os trimestres
publico_alvo_trim1 <- dados_pnadc_2024_trim1 %>%
  filter(V2009 >= 14 & V2009 <= 24)

publico_alvo_trim2 <- dados_pnadc_2024_trim2 %>%
  filter(V2009 >= 14 & V2009 <= 24)

# Adicionando identificador do domicílio
publico_alvo_trim1 <- publico_alvo_trim1 %>%
  mutate(ID_DOMICILIO = paste0(UPA, "_", V1014, "_", V1008))

publico_alvo_trim2 <- publico_alvo_trim2 %>%
  mutate(ID_DOMICILIO = paste0(UPA, "_", V1014, "_", V1008))

# Calculando a renda per capita por domicílio para o 1º trimestre
renda_domicilio_trim1 <- publico_alvo_trim1 %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    renda_total_domicilio = sum(VD4019, na.rm = TRUE),
    num_pessoas_domicilio = n()
  ) %>%
  mutate(renda_per_capita = renda_total_domicilio / num_pessoas_domicilio)

# Calculando a renda per capita por domicílio para o 2º trimestre
renda_domicilio_trim2 <- publico_alvo_trim2 %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    renda_total_domicilio = sum(VD4019, na.rm = TRUE),
    num_pessoas_domicilio = n()
  ) %>%
  mutate(renda_per_capita = renda_total_domicilio / num_pessoas_domicilio)

# Juntando a renda per capita aos dados de cada indivíduo no 1º trimestre
publico_alvo_trim1 <- publico_alvo_trim1 %>%
  left_join(
    renda_domicilio_trim1 %>% select(ID_DOMICILIO, renda_per_capita),
    by = "ID_DOMICILIO"
  )

# Juntando a renda per capita aos dados de cada indivíduo no 2º trimestre
publico_alvo_trim2 <- publico_alvo_trim2 %>%
  left_join(
    renda_domicilio_trim2 %>% select(ID_DOMICILIO, renda_per_capita),
    by = "ID_DOMICILIO"
  )

# Criando dummies
publico_alvo_trim1 <- publico_alvo_trim1 %>%
  mutate(
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),
    renda_baixa_dummie = ifelse(renda_per_capita <= 706, 1, 0)
  )

publico_alvo_trim2 <- publico_alvo_trim2 %>%
  mutate(
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),
    renda_baixa_dummie = ifelse(renda_per_capita <= 706, 1, 0)
  )

# Visualizando os dados processados para o 1º e 2º trimestre
publico_alvo_trim1
publico_alvo_trim2

# Criando o identificador único do indivíduo em ambos os trimestres
publico_alvo_trim1 <- publico_alvo_trim1 %>%
  mutate(
    id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
    trimestre = "2024_1"  # Identifica o trimestre
  )

publico_alvo_trim2 <- publico_alvo_trim2 %>%
  mutate(
    id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
    trimestre = "2024_2"  # Identifica o trimestre
  )

# Combinando as bases dos dois trimestres
publico_alvo_painel <- bind_rows(publico_alvo_trim1, publico_alvo_trim2)

# Movendo as colunas `id_individuo`, `V1002` (ID do município), `ensino_medio_dummie`, e `renda_baixa_dummie` para o início
publico_alvo_painel <- publico_alvo_painel %>%
  select(id_individuo, , ensino_medio_dummie, renda_baixa_dummie, everything())

# Visualizando o resultado final
publico_alvo_painel

# Renomeando colunas para remover caracteres especiais, como pontos (.)
colnames(publico_alvo_painel) <- gsub("[^[:alnum:]_]", "_", colnames(publico_alvo_painel))



# Filtrando o painel para manter apenas indivíduos que aparecem nos dois trimestres
publico_alvo_painel_completo <- publico_alvo_painel %>%
  group_by(id_individuo) %>%
  filter(n_distinct(trimestre) == 2) %>%  # Garante que o indivíduo está presente nos dois trimestres
  ungroup()

# Visualizando o painel final sem indivíduos faltantes em um dos trimestres
publico_alvo_painel_completo


# Adicionando a variável `deserção` para marcar os indivíduos que estavam no ensino médio no 1º trimestre mas não no 2º trimestre
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  group_by(id_individuo) %>%
  mutate(
    deserção = ifelse(Trimestre == 1 & ensino_medio_dummie == 1 & 
                        any(Trimestre == 2 & ensino_medio_dummie == 0), 1, 0)
  ) %>%
  ungroup()

# Selecionando o último registro para cada id_individuo para evitar duplicidade
publico_alvo_dados_probit <- publico_alvo_painel_completo %>%
  filter(Trimestre == 1) %>%
  select(
    id_individuo, deserção, renda_per_capita, V1022, V2001, V2007, V2009, V2010, UF, VD2004, VD3004, VD4019
  )

# Transformando a variável `deserção` em fator binário
publico_alvo_dados_probit <- publico_alvo_dados_probit %>%
  mutate(deserção = as.factor(deserção))

# Instalando e carregando o pacote para regressão Probit, se necessário
if(!require("MASS")) install.packages("MASS", dependencies = TRUE)
library(MASS)

# Executando o modelo Probit
modelo_probit <- glm(deserção ~ renda_per_capita + V1022 + V2001 + V2007 + V2009 + V2010 + VD2004 + VD3004 + VD4019, 
                     data = publico_alvo_dados_probit, family = binomial(link = "probit"))

# Resumo do modelo Probit
summary(modelo_probit)

# Extraindo os coeficientes e intervalos de confiança para plotar
tidy_model <- tidy(modelo_probit, conf.int = TRUE)

# Gráfico de Coeficientes com Intervalos de Confiança
ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Coeficientes do Modelo Probit com Intervalos de Confiança",
       x = "Variável",
       y = "Estimativa do Coeficiente") +
  coord_flip() +
  theme_minimal()

## Variaveis de interesse
variaveis_interesse <- c('UPA','V1008', 'V1014','V2003',"V1022", "V2001", "V2007", "V2009", "V2010", "UF", "VD2004", "VD3004", "VD4019"," V3001" )
