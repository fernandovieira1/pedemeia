### DADOS PNAD -- Análise exploratória

######################## 2. BASE ABANDONO ########################

### 2.1 base_abandono (DF) ####
# Abandono compara 1 com 2 tri, 2 com 3 tri, 3 com 4 tri.

## *Criar Coluna id_individuo ####
# Seleciona apenas as colunas necessárias desde o início
base_abandono <- publico_alvo_filtrado %>%
  transmute(
    id_individuo = paste0(UPA, '_', V1008, '_', V1014, '_', V2003, '_', V2008, '_', V20081, '_', V20082),
    ID_DOMICILIO, V2009, V2001, VD2002, Ano, Trimestre, V3002, V3003A, V3006, VD4020,
    VD3005, V2007, UPA, V20082, V2010, V1022, V3002A, VD4013, Estrato, V1028032, VD2004
  )

# Libera memória
rm(publico_alvo_filtrado)
gc()

## *Ordenar por ID_DOMICILIO, ano e trimestre ####
base_abandono <- base_abandono %>%
  arrange(ID_DOMICILIO, Ano, Trimestre)
gc()

## *Critérios Pé-de-Meia ####
base_abandono <- base_abandono %>%
  mutate(
    ensino_medio = ifelse(
      V2009 >= 14 & V2009 <= 24 &
        V3002 == 'Sim' &
        V3003A == 'Regular do ensino médio',
      1, 0
    )
  )
gc()

## *Calcular RD (Renda Domiciliar) e RDPC (Renda Domiciliar Per Capita) ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE),
    RDPC = round(RD / V2001, 0)
  ) %>%
  ungroup()
gc()

## *Criar dummy renda per capita < 1/2 Sal. Mín. ####
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
}

base_abandono <- base_abandono %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = as.integer(RDPC < (salario_minimo / 2))
  )
gc()

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
  )
gc()

## *Identificar a educação da mãe e do pai ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    is_mae = as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()
gc()

## *Criar variável de abandono ####
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre) %>%
  group_by(id_individuo) %>%
  filter(n() > 1) %>%
  mutate(
    abandono_cond_1 = Trimestre == 2 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 3 & dplyr::lead(ensino_medio) == 1),
    abandono_cond_2 = Trimestre == 3 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 4 & dplyr::lead(ensino_medio) == 1),
    abandono_cond_3 = Trimestre == 4 & ensino_medio == 1 &
      !(dplyr::lead(Trimestre) == 4 & dplyr::lead(ensino_medio) == 1),
    abandono = ifelse(
      Trimestre > 1 & (abandono_cond_1 | abandono_cond_2 | abandono_cond_3),
      1, 0
    )
  ) %>%
  ungroup() %>%
  select(-abandono_cond_1, -abandono_cond_2, -abandono_cond_3)
gc()

## *Filtrar observações com dados inconsistentes ####
# base_abandono <- base_abandono %>%
#   filter(V20082 != 9999)
# gc()


### 2.2 df Abandono Filtrado ####
base_abandono_filtrada <- base_abandono %>%
  select(
    id_individuo,
    ID_DOMICILIO,
    Estrato,
    V1028032,
    V2009, # Idade
    V2010,
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
    V3002, V3003A, V3006, VD3005, V2007, UPA, V20082, 
    V1022, V3002A, VD4013, VD4020, VD2004
  )
gc()

# Resultados finais
cat('Processamento concluído. Dados prontos para análise!\n')
