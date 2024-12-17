# Carregando pacotes necessários
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
library(rlang)
library(reactable)
library(ggplot2)
library(broom)
library(base)

# Limpando o ambiente
rm(list=ls(all=TRUE))

# Importando os dados do 1º e 2º trimestres de 2024
dados_pnadc_2024_trim1 <- get_pnadc(year = 2024, quarter = 1, design = FALSE)
dados_pnadc_2024_trim2 <- get_pnadc(year = 2024, quarter = 2, design = FALSE)

## Calculando a rendimeno domiciliar per capita  para ambos os trimestres

## Criando dummies para o público-alvo e outras características
dados_pnadc_2024_trim1 <- dados_pnadc_2024_trim1 %>%
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0),
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),  # Regular do ensino médio
    residencia_unipessoal = ifelse(VD2004 == "Unipessoal", 1, 0),  # Unidade doméstica unipessoal
    #renda_baixa_dummie = ifelse(renda_per_capita <= 706, 1, 0),  # Renda per capita <= 706 reais
    rede_publica = ifelse(V3002A == "Rede pública", 1, 0)  # Rede pública
  )

dados_pnadc_2024_trim2 <- dados_pnadc_2024_trim2 %>%
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0),
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),
    residencia_unipessoal = ifelse(VD2004 == "Unipessoal", 1, 0),
    #renda_baixa_dummie = ifelse(renda_per_capita <= 706, 1, 0),
    rede_publica = ifelse(V3002A == "Rede pública", 1, 0)
  )

# Criando o identificador único do indivíduo e marcando o trimestre
dados_pnadc_2024_trim1 <- dados_pnadc_2024_trim1 %>%
  mutate(
    id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
    Trimestre = "2024_1"
  )

dados_pnadc_2024_trim2 <- dados_pnadc_2024_trim2 %>%
  mutate(
    id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
    Trimestre = "2024_2"
  )

# Combinando as bases dos dois trimestres
publico_alvo_painel <- bind_rows(dados_pnadc_2024_trim1, dados_pnadc_2024_trim2)


# Combinando as bases dos dois trimestres
publico_alvo_painel <- bind_rows(publico_alvo_trim1, publico_alvo_trim2)

# Filtrando o painel para manter apenas indivíduos que aparecem nos dois trimestres
publico_alvo_painel_completo <- publico_alvo_painel %>%
  group_by(id_individuo) %>%
  filter(n_distinct(Trimestre) == 2) %>%  # Garante que o indivíduo está presente nos dois trimestres
  ungroup()

# Adicionando a variável `abandono` para marcar os indivíduos que estavam no ensino médio no 1º trimestre mas não no 2º trimestre
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  group_by(id_individuo) %>%
  mutate(
    abandono = ifelse(Trimestre == 1 & ensino_medio_dummie == 1 & 
                        any(Trimestre == 2 & ensino_medio_dummie == 0), 1, 0)
  ) %>%
  ungroup()

# Certificando-se de usar a função select do pacote dplyr
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  dplyr::select(
    id_individuo, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal, rede_publica, abandono, 
    everything()
  )


# Removendo observações onde V20082 é igual a 9999
publico_alvo_painel_completo <- publico_alvo_painel_completo %>%
  filter(V20082 != 9999)



## Variaveis de interesse
variaveis_interesse <- c("UPA","V1008","V1014","Ano","Trimestre","UF","Capital","RM_RIDE","V1022","V2005",
                                                      "V2007","V2009","V2010","V3002","V3002A","V3003A","V3010","V3013","VD2002","VD2006","VD2004","VD3004","VD4019",
                                                      "VD4048","VD3005","VD4001","VD4002","VD4003","VD4014","VD4009","VD4020","VD4047","V5001A","V5002A","V5003A", "educ mãe", "educpai", "maxeducmaeepai", "Estado"   )


saveRDS(publico_alvo_painel_completo, file = "publico_alvo_painel_teste.rds")
