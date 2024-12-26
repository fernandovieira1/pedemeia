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

## *Ordenar por id_individuo, ano e trimestre ####
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre)

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
  group_by(ID_DOMICILIO) %>%
  mutate(
    RD = sum(VD4020, na.rm = TRUE), # Rendimento domiciliar total
  ) %>%
  ungroup()

## *Calcular RDPC (Renda Domiciliar Per Capita) ####
base_abandono <- base_abandono %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    RDPC = RD/V2001, # V2001: Qtde residentes no domicílio
  ) %>%
  ungroup()
base_abandono$RDPC <- round(base_abandono$RDPC, 0)

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

## *Criar a dummy de abandono ####
base_abandono <- base_abandono %>%
  # Transformar 'Trimestre', 'Ano' e 'V3003A' para os tipos adequados
  mutate(
    Trimestre = as.integer(Trimestre),
    Ano = as.integer(Ano),
    V3003A = as.character(V3003A)  # Garantir que 'V3003A' seja comparável
  ) %>%
  # Filtrar apenas indivíduos com mais de uma entrada válida
  group_by(id_individuo) %>%
  filter(n_distinct(Trimestre) > 1) %>%
  # Ordenar os dados por Ano e Trimestre dentro de cada indivíduo
  arrange(Ano, Trimestre, .by_group = TRUE) %>%
  # Criar a dummy de abandono
  mutate(
    abandono = ifelse(
      # Apenas considerar trimestres 2, 3 e 4 para cálculo de abandono
      Trimestre > 1 & (
        # Condição 1: Entre T1 e T2 do mesmo ano
        (Trimestre == 2 & V3003A == 'Regular do ensino médio' &
           !(dplyr::lead(Trimestre, default = NA_integer_) == 3 &
               dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
          # Condição 2: Entre T2 e T3 do mesmo ano
          (Trimestre == 3 & V3003A == 'Regular do ensino médio' &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 4 &
                 dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio')) |
          # Condição 3: Entre T3 e T4 do mesmo ano
          (Trimestre == 4 & V3003A == 'Regular do ensino médio' &
             !(dplyr::lead(Trimestre, default = NA_integer_) == 1 &
                 dplyr::lead(Ano, default = NA_integer_) == Ano + 1 &
                 dplyr::lead(V3003A, default = 'NA') == 'Regular do ensino médio'))
      ),
      1,  # Marca como abandono
      0   # Caso contrário, não há abandono
    )
  ) %>%
  # Remover o agrupamento
  ungroup()
# Obs.: demora o processamento aqui

## Verificar abandono por trimestre
cat('Distribuição por trimestre e abandono:\n')
base_abandono %>%
  filter(!is.na(abandono)) %>%  
  count(Trimestre, abandono) %>%
  pivot_wider(
    names_from = abandono,
    values_from = n,
    names_prefix = 'Absoluto_'
  ) %>%
  mutate(
    Absoluto_0 = replace_na(Absoluto_0, 0),  
    Absoluto_1 = replace_na(Absoluto_1, 0),
    Relativo_1 = round((Absoluto_1 / (Absoluto_0 + Absoluto_1)) * 100, 5)  
  ) %>%
  select(Trimestre, Absoluto_0, Absoluto_1, Relativo_1) %>% 
  arrange(Trimestre)

## Verificar taxa de abandono
cat('\nTaxa de abandono (4 trimestres):\n')
# print(prop.table(table(base_abandono$abandono)))

# table(base_abandono$Ano)
# table(base_abandono$Trimestre)

### 2.2 df Abandono Filtrado ####
# Reorganizando as colunas para trazer as novas variáveis para o começo
base_abandono_filtrada <- base_abandono %>%
  select( 
    id_individuo,
    Ano, 
    Trimestre, 
    ensino_medio,
    VD4020, # Rendimento mensal todos os trabalhos
    RD, 
    RDPC, 
    RDPC_menor_meio_sm, 
    regiao, 
    educacao_mae, 
    educacao_pai, 
    abandono,
    salario_minimo, 
    everything()
  )

## Removendo observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

# table(base_abandono_filtrada$abandono)
# prop.table(round(table(base_abandono_filtrada$abandono)))
# O percentual de abandono escolar (todos os períodos) pode ser visto aqui
