### DADOS PNAD -- Análise exploratória

######################## 2. BASE ABANDONO ########################

### 2.1 base_abandono (DF) ####
# Abandono compara 1 com 2 tri, 2 com 3 tri, 3 com 4 tri.
base_abandono <- publico_alvo_filtrado %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  )

## *Criar Coluna id_individuo ####
base_abandono <- base_abandono %>%
  mutate(
    id_individuo = paste0(UPA, '_', # Unidade Primária de Amostragem
                          V1008, '_', # Nr. de diferenciação de domicílios na mesma UPA
                          V1014, '_', # Domicílios que permanecem na amostra da PNAD
                          V2003, '_', # Nr. de ordem (de registro na pnad) do morador no domicílio
                          V2008, '_', # Dia de nascimento
                          V20081, '_', # Mês de nascimento
                          V20082) # Ano de nascimento
  )

## *Ordenar por ID_DOMICILIO, ano e trimestre ####
base_abandono <- base_abandono %>%
  arrange(ID_DOMICILIO, Ano, Trimestre)

## *Critérios Pé-de-Meia ####
# V2009: Idade
# V3003A: qual curso frequenta
# VD2004: Condição de ocupação do domicílio
base_abandono <- base_abandono %>%
  mutate(
    # Critério unificado para Geral
    ensino_medio = ifelse(
      (V2009 >= 14 & V2009 <= 24 & 
         V3002A == 'Rede pública' & 
         V3003A == 'Regular do ensino médio'),
      1, 0
    )
  ) # (?) Considerar 'NA's' como 0?
# EJA retirado em modelo_pdm7 / inserir depois

## *Calcular RD (Renda Domiciliar) #####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE), # Rendimento domiciliar total
  ) %>%
  ungroup()

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(
    RDPC = RD/V2001, # V2001: Qtde residentes no domicílio
  ) %>%
  ungroup()
base_abandono$RDPC <- round(base_abandono$RDPC, 0)

## *Criar dummy renda per capita < 1/2 Sal. Mín. ####
# Função salários mínimos
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

# Criar a variável dummy sm
base_abandono <- base_abandono %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = if_else(RDPC < (sal_min(Ano)/2), 1, 0) 
  )

## *Adicionar a coluna de região ####
base_abandono <- base_abandono %>%
  mutate(
    regiao = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ))
# table(base_abandono$regiao)
# prop.table(table(base_abandono$regiao))

## *Identificar a educação da mãe e do pai ####
# V2007: Sexo
# VD2002: Condição no domicílio (01: Condição no domicílio; 02: Cônjuge ou companheiro(a); 06: Pai, mãe, padrasto ou madrasta)
# VD3005: Grau de instrução (ensino fund. com 9 anos)
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%  # Substitua ID_DOMICILIO pelo identificador do grupo familiar, se for diferente
  mutate(
    is_mae = as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()

## *Criar a dummy de abandono escolar ####
base_abandono <- base_abandono %>%
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)
  ) %>%
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  mutate(
    abandono = ifelse(
      Trimestre > 1 & (
        (Trimestre == 2 & ensino_medio == 1 &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 3 &
               dplyr::lead(ensino_medio) == 1)) |
          (Trimestre == 3 & ensino_medio == 1 &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 4 &
                 dplyr::lead(ensino_medio) == 1)) |
          (Trimestre == 4 & ensino_medio == 1 &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 4 &
                 dplyr::lead(ensino_medio) == 1))
      ),
      1,  # Marca como abandono
      0   # Caso contrário, não há abandono
    )
  )

## Verificar abandono por trimestre
cat('Distribuição por trimestre e abandono:\n')
base_abandono %>%
  filter(!is.na(abandono)) %>%  # Exclui NAs em 'abandono'
  count(Trimestre, abandono) %>%  # Conta a frequência por trimestre e abandono
  pivot_wider(
    names_from = abandono,
    values_from = n,
    names_prefix = 'Absoluto_'
  ) %>%
  # Garante que as colunas Absoluto_0 e Absoluto_1 existam
  mutate(
    Absoluto_0 = ifelse(is.na(Absoluto_0), 0, Absoluto_0),
    Absoluto_1 = ifelse(is.na(Absoluto_1), 0, Absoluto_1),
    Relativo_1 = round((Absoluto_1 / (Absoluto_0 + Absoluto_1)) * 100, 5)  
  ) %>%
  select(Trimestre, Absoluto_0, Absoluto_1, Relativo_1) %>% 
  arrange(Trimestre)

## Verificar taxa de abandono
cat('\nTaxa de abandono (4 trimestres):\n')
print(prop.table(table(base_abandono$abandono)))

# table(base_abandono$Ano)
# table(base_abandono$Trimestre)

### 2.2 df Abandono Filtrado ####
# Reorganizando as colunas para trazer as novas variáveis para o começo
base_abandono_filtrada <- base_abandono %>%
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

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

# table(base_abandono_filtrada$abandono)
# prop.table(round(table(base_abandono_filtrada$abandono)))
# O percentual de abandono escolar (todos os períodos) pode ser visto aqui
