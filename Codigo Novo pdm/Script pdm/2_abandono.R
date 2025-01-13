### DADOS PNAD -- Análise exploratória

######################## 2. BASE ABANDONO ########################

### 2.1 base_abandono (DF) ####
# Abandono compara 1 com 2 tri, 2 com 3 tri, 3 com 4 tri.

## *Criar Coluna id_individuo ####
base_abandono <- publico_alvo_filtrado %>%
  transmute( # Use transmute para criar apenas a coluna necessária
    id_individuo = paste0(UPA, '_', V1008, '_', V1014, '_', V2003, '_', V2008, '_', V20081, '_', V20082),
    across(everything()) # Preserve as outras colunas
  )

rm(publico_alvo_filtrado)
gc() # rápido

## *Ordenar por ID_DOMICILIO, ano e trimestre ####
# Use arrange diretamente com as colunas necessárias
base_abandono <- base_abandono %>%
  arrange(ID_DOMICILIO, Ano, Trimestre)
gc() # demora

## *Critérios Pé-de-Meia ####
# Evite reprocessamento e use mutate com lógica simplificada
base_abandono <- base_abandono %>%
  mutate(
    ensino_medio = ifelse(
      V2009 >= 14 & V2009 <= 24 &
        VD2006 %in% c('14 a 19 anos', '20 a 24 anos') &
        V3002 == 'Sim' &
        V3003A == 'Regular do ensino médio',
      1, 0
    )
  ) # rápido

## *Calcular RD (Renda Domiciliar) #####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(RD = sum(VD4020, na.rm = TRUE)) %>%
  ungroup()
gc() # rápido

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_abandono <- base_abandono %>%
  mutate(RDPC = round(RD / V2001, 0)) # Combine cálculo e arredondamento em um passo
gc() # rápido

## *Criar dummy renda per capita < 1/2 Sal. Mín. ####
# Função salários mínimos - otimizada para vetores
sal_min <- function(ano) {
  case_when(
    ano == 2024 ~ 1412,
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
} # rápido

# Criar dummy RDPC menor que meio salário mínimo
base_abandono <- base_abandono %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = as.integer(RDPC < (salario_minimo / 2))
  ) # rápido

## *Adicionar a coluna de região ####
base_abandono <- base_abandono %>%
  mutate(
    regiao = case_when(
      substr(UPA, 1, 1) == '1' ~ 'Norte',
      substr(UPA, 1, 1) == '2' ~ 'Nordeste',
      substr(UPA, 1, 1) == '3' ~ 'Sudeste',
      substr(UPA, 1, 1) == '4' ~ 'Sul',
      substr(UPA, 1, 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    )
  ) # rápido
gc()

## *Identificar a educação da mãe e do pai ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    is_mae = V2007 == 2 & VD2002 %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), first(VD3005[is_mae]), NA),
    is_pai = V2007 == 1 & VD2002 %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), first(VD3005[is_pai]), NA)
  ) %>%
  ungroup() # demora
gc()

## *Organizar em ordem ascendente por id, ano e trimestre ####
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre) # demora
gc()

## Transformar colunas para os tipos adequados
# base_abandono <- base_abandono %>%
#   mutate(
#     Trimestre = as.integer(Trimestre),
#     Ano = as.integer(Ano),
#     V3003A = as.character(V3003A),
#     V3006 = as.character(V3006)
#   )

## CRIAR ABANDONO ####
gc()

# Filtrar indivíduos com mais de uma entrada
base_abandono <- base_abandono %>%
  group_by(id_individuo) %>%
  filter(n() > 1) %>%
  ungroup() # demora muito
gc()

# Ordenar os dados por Ano e Trimestre
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre) # demora muito
gc()

# Dividir a criação da coluna 'abandono' em partes
base_abandono_filtrada <- base_abandono %>%
  mutate(
    abandono_cond_1 = Trimestre == 2 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 3 & dplyr::lead(ensino_medio) == 1),
    abandono_cond_2 = Trimestre == 3 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 4 & dplyr::lead(ensino_medio) == 1),
    abandono_cond_3 = Trimestre == 4 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 4 & dplyr::lead(ensino_medio) == 1)
  ) # rápido
gc()

# Criar a coluna final de 'abandono' com base nas condições
base_abandono_filtrada <- base_abandono_filtrada %>%
  mutate(
    abandono = ifelse(
      Trimestre > 1 & (abandono_cond_1 | abandono_cond_2 | abandono_cond_3),
      1, 0
    )
  ) %>%
  select(-abandono_cond_1, -abandono_cond_2, -abandono_cond_3) # rápido
gc()

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999) # demora muito
gc()

### 2.2 df Abandono Filtrado ####
base_abandono_filtrada <- base_evasao_filtrada %>%
  select(
    id_individuo,
    ID_DOMICILIO,
    V2009, # Idade
    V2001,
    VD2002,
    Ano,
    Trimestre,
    ensino_medio,
    VD4020, # Rendimento mensal todos os trabalhos
    RD,
    RDPC,
    RDPC_menor_meio_sm,
    abandono,
    regiao,
    educacao_mae,
    educacao_pai,
    salario_minimo,
    everything()
  )

# Resultados finais
cat('Processamento concluído. Dados prontos para análise!\n')
