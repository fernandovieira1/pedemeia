### DADOS PNAD -- Análise exploratória


######################## 1. BASE EVASÃO ########################

### 1.1 base_evasao (DF) ####
# Evasão compara T1 do ano A com T1 do ano A+1

## *Criar base_evasao ####
base_evasao <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1)
# table(base_evasao$Trimestre)

## *Criar Coluna id_individuo ####
base_evasao <- base_evasao %>%
  mutate(
    id_individuo = paste0(UPA, '_', # Unidade Primária de Amostragem
                          V1008, '_', # Nr. de diferenciação de domicílios na mesma UPA
                          V1014, '_', # Grupo de amostra  
                          V2003, '_', # Nr. de ordem (de registro na pnad) do morador no domicílio
                          V2008, '_', # Dia de nascimento
                          V20081, '_', # Mês de nascimento
                          V20082) # Ano de nascimento
  )

## *Ordenar por ID_DOMICILIO, ano e trimestre ####
base_evasao <- base_evasao %>%
  arrange(ID_DOMICILIO, Ano, Trimestre) 

## *Critérios Pé-de-Meia ####
# V2009: Idade
# VD2006: Faixa etária
# V3002: Frequenta escola
# V3002A: frequenta escola regular
# V3003A: qual curso frequenta
base_evasao <- base_evasao %>%
  mutate(
    ensino_medio = ifelse(
      !is.na(V2009) & !is.na(VD2006) & !is.na(V3002) & !is.na(V3003A) & !is.na(V3006) & 
        V2009 >= 14 & V2009 <= 24 &
        (VD2006 == '14 a 19 anos' | VD2006 == '20 a 24 anos') &
        V3002 == 'Sim' & # en algum momento respondeu Sim
        V3003A == 'Regular do ensino médio',
      1, 0
    )
  ) 

## *Calcular RD (Renda Domiciliar) #####
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE), # Rendimento domiciliar total
  ) %>%
  ungroup()

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO, Ano) %>%
  mutate(
    RDPC = RD/V2001, # V2001: Qtde residentes no domicílio
  ) %>%
  ungroup()
base_evasao$RDPC <- round(base_evasao$RDPC, 0)

# Criar a variável dummy sm/2
base_evasao <- base_evasao %>%
  mutate(
    salario_minimo = sal_min(Ano),
    RDPC_menor_meio_sm = if_else(RDPC < (sal_min(Ano)/2), 1, 0) 
  )

## *Adicionar a coluna de região ####
base_evasao <- base_evasao %>%
  mutate(
    regiao = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ))
# table(base_evasao$regiao)
# prop.table(table(base_evasao$regiao))

## *Identificar a educação da mãe e do pai ####
# V2007: Sexo
# VD2002: Condição no domicílio (01: Condição no domicílio; 02: Cônjuge ou companheiro(a); 06: Pai, mãe, padrasto ou madrasta)
# VD3005: Grau de instrução (ensino fund. com 9 anos)
base_evasao <- base_evasao %>%
  group_by(ID_DOMICILIO) %>%  # Substitua ID_DOMICILIO pelo identificador do grupo familiar, se for diferente
  mutate(
    is_mae = as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()

# base_evasao %>%
#   select(educacao_mae, educacao_pai) %>%
#   summary()

## *Organizar em ordem ascendente por id, ano e trimestre ####
base_evasao <- base_evasao %>% 
  arrange(id_individuo, Ano, Trimestre)

## ## CRIAR EVASÃO ####
base_evasao <- base_evasao %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A),  # Garantir que 'V3003A' seja comparável
    V3006 = as.character(V3006)     # Garantir que 'V3006' seja comparável (série do aluno)
  ) %>%
  # Filtrar apenas indivíduos com mais de uma entrada
  group_by(id_individuo) %>%
  filter(n() > 1) %>%
  # Ordenar os dados por Ano e Trimestre dentro de cada indivíduo
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  # Criar a dummy de evasão
  mutate(
    evasao = ifelse(
      # Condição principal: Matriculado no 1º trimestre do ano T, mas não aparece no 1º trimestre do ano T+1
      # Exclui os alunos do terceiro ano (V3006 == 'Terceira (o)')
      Trimestre == 1 & ensino_medio == 1 & V3006 != 'Terceira (o)' &
        !(dplyr::lead(Trimestre, default = NA_integer_) == 1 &
            dplyr::lead(Ano, default = NA_integer_) == Ano + 1 &
            dplyr::lead(ensino_medio) == 1),
      1,  # Marca como evasão
      0   # Caso contrário, não há evasão
    )
  ) %>%
  # Remover o agrupamento
  ungroup()


base_evasao <- base_evasao %>%
  mutate(
    evasao = replace_na(evasao, 1)  # Substitui NA por 1
  )

### 1.2 df Evasão Filtrado ####
# - Filtrar os indivíduos que responderam tanto no T1 do ano T quanto no T1 do ano T+1,
# considerando indivíduos com continuidade de presença em dois anos consecutivos no mesmo 
# trimestre (1º Trimestre).
base_evasao_filtrada <- base_evasao %>%
  filter(
    Ano %in% anos & Trimestre == 1
  ) %>%
  group_by(id_individuo) %>%
  summarise(anos = list(unique(Ano))) %>%
  filter(any(diff(sort(unlist(anos))) == 1))

# Merge para incluir as variáveis do dataframe original
base_evasao_filtrada <- base_evasao %>%
  inner_join(base_evasao_filtrada, by = 'id_individuo')

## Removendo observações onde V20082 é igual a 9999
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(V20082 != 9999) # V20082: Ano de nascimento

# Reorganizando as colunas para trazer as novas variáveis para o começo
base_evasao_filtrada <- base_evasao_filtrada %>%
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
    evasao,
    regiao, 
    educacao_mae, 
    educacao_pai, 
    salario_minimo, 
    everything()
  )

