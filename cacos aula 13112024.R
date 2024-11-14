install.packages("plm")
install.packages("glmnet")
install.packages("fixest")
install.packages("plm")
install.packages("janitor")


# Carregando pacotes necessários
library(PNADcIBGE)
library(convey)
library(tidyverse)
library(gt)
library(scales)
library(dplyr)
library(haven)
library(fixest)
library(lme4)
library(plm)
library(janitor)
library(haven)
library(stargazer)
library(gt)


# Limpando o ambiente
rm(list=ls(all=TRUE))
gc()


####################################### Construindo a base de dados##############################################
### 0.3 Definir anos e trimestres ####
# Defina o diretório onde o arquivo .RDS está armazenado
diretorio_dados <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'

# Nome do arquivo com todos os dados de 2022 e 2023
nome_arquivo <- file.path(diretorio_dados, 'dados_pnad_2022-2023.rds')

# Definir anos e trimestres que você deseja filtrar
anos <- c(2022, 2023)     # Pode especificar um ou mais anos
trimestres <- c(1, 2)     # Pode especificar um ou mais trimestres

# Carregar o arquivo de dados completo
dados_pnad <- tryCatch({
  readRDS(nome_arquivo)
}, error = function(e) {
  warning('Erro ao carregar o arquivo de dados.')
  NULL
})

# Verifique se o carregamento foi bem-sucedido
if (!is.null(dados_pnad)) {
  dados_pnad <- as.data.frame(dados_pnad)
  
  # Filtrar para os anos e trimestres desejados
  dados_pnad <- dados_pnad %>%
    filter(Ano %in% anos & Trimestre %in% trimestres)
  
} else {
  warning('Os dados não foram carregados corretamente.')
}

# Verificar a distribuição de anos e trimestres no filtro aplicado
table(dados_pnad$Ano)
table(dados_pnad$Trimestre)

## Variáveis/Colunas PNADc
variaveis_interesse <- c(
  'UPA', 'V1008', 'V1014', 'V2003', 'V2007', 'V2008', 'V2010', 'V20081', 'V20082', 'V2009',
  'V3003A', 'VD2004', 'V3002A', 'ID_DOMICILIO', 'V2001', 'VD4016',
  'VD4017', 'VD2003', 'VD4019', 'V2007', 'V3009A','VD2002', 'V3006','VD3005', 
  'V2010', 'VD4020','V3002', 'Ano', 'Trimestre')

## Definir df 
publico_alvo_filtrado <- dados_pnad %>%
  select(all_of(variaveis_interesse))

#### 1. BASE evasao ####
### 1.1 df evasao ####
# Critérios
base_evasao <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1)
table(base_evasao$Trimestre)

# Corrigir a sintaxe da função mutate
base_evasao <- base_evasao %>%
  mutate(
    # Criar a coluna id_individuo
    id_individuo = paste0(UPA, "", V1008, "", V1014, "", V2003, "", V2008, "", V20081, "", V20082)
  ) %>%
  # Organizar a base pelos identificadores e trimestres
  arrange(id_individuo, Ano, Trimestre) %>%
  # Adicionar as outras colunas e variáveis necessárias
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0),
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),
    residencia_unipessoal = ifelse(VD2004 == "Unipessoal", 1, 0),
    rede_publica = ifelse(V3002A == "Rede pública", 1, 0),
    RD = VD4020,
    V2001R = as.numeric(V2001),
    RDPC = RD / V2001R,
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0),
    região = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ),
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  select(-is_mae, -is_pai)

# Criando a variável de transição usando lag() com variáveis do tipo factor
base_evasao <- base_evasao %>%
  arrange(id_individuo, Ano) %>%  # Ordena a base por id e ano
  group_by(id_individuo) %>%
  mutate(
    evasao = ifelse(
      first(V3002) == "Sim" & last(V3002) == "Não", 1, 0
    )
  ) %>%
  ungroup()


# Filtrar os indivíduos que responderam tanto no T1 do ano T quanto no T1 do ano T+1
base_evasao_filtrada <- base_evasao %>%
  group_by(id_individuo) %>%
  filter(
    (any(Ano == 2022 & Trimestre == 1) & any(Ano == 2023 & Trimestre == 1)) |
      (any(Ano == 2023 & Trimestre == 1) & any(Ano == 2024 & Trimestre == 1))
  ) %>%
  ungroup()

# Reorganizando as colunas para trazer as novas variáveis para o começo
base_evasao_filtrada <- base_evasao_filtrada %>%
  select(
    id_individuo, Ano, Trimestre, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal,
    rede_publica, renda_per_capta_menor_706, RDPC, região, educacao_mae, educacao_pai, evasao,
    everything()
  )

# Removendo observações onde V20082 é igual a 9999
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(V20082 != 9999)

#######################################  Criando base abandono ###################################################

# Critérios
base_abandono <- publico_alvo_filtrado %>% 
  filter(Trimestre == 1 | Trimestre == 2)
table(base_abandono$Trimestre)

# Carregar a base base_abandono
# base_abandono <- readRDS("base_abandono")


# Filtrar a base para as variáveis de interesse
base_abandono <- base_abandono[, variaveis_interesse]

# Adicionar as novas colunas na base base_abandono
base_abandono <- base_abandono %>%
  # Criar a coluna id_individuo
  mutate(
    id_individuo = paste0(UPA, "", V1008, "", V1014, "", V2003, "", V2008, "", V20081, "", V20082)
  ) %>%
  # Organizar a base pelos identificadores e trimestres
  arrange(id_individuo, Ano, Trimestre) %>%
  # Adicionar as outras colunas e variáveis necessárias
  mutate(
    faixa_idade_14_24 = ifelse(V2009 >= 14 & V2009 <= 24, 1, 0),
    ensino_medio_dummie = ifelse(V3003A == "Regular do ensino médio", 1, 0),
    residencia_unipessoal = ifelse(VD2004 == "Unipessoal", 1, 0),
    rede_publica = ifelse(V3002A == "Rede pública", 1, 0),
    
    # Rendimento domiciliar total
    RD = VD4020,  # Rendimento domiciliar total por domicílio
    
    # Número de pessoas no domicílio
    V2001R = as.numeric(V2001),  # Convertendo V2001 para numérico
    
    # Calcular a Renda Domiciliar Per Capita (RDPC)
    RDPC = RD / V2001R,
    
    # Criar a dummy para renda per capita menor que 706
    renda_per_capta_menor_706 = ifelse(RDPC < 706, 1, 0),
    
    # Adicionar a coluna de região
    região = case_when(
      substr(UPA, start = 1, stop = 1) == '1' ~ 'Norte',
      substr(UPA, start = 1, stop = 1) == '2' ~ 'Nordeste',
      substr(UPA, start = 1, stop = 1) == '3' ~ 'Sudeste',
      substr(UPA, start = 1, stop = 1) == '4' ~ 'Sul',
      substr(UPA, start = 1, stop = 1) == '5' ~ 'Centro-Oeste',
      TRUE ~ NA_character_
    ),
    
    # Identificar a educação da mãe e do pai
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA),
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6))),
    educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)
  ) %>%
  ungroup()  # Remove o agrupamento após a criação das colunas

# Filtrar quem respondeu os 2 trimestres
base_abandono <- base_abandono %>%
  arrange(id_individuo, Ano, Trimestre) %>%  # Ordena a base por id, ano e trimestre
  group_by(id_individuo, Ano) %>%
  mutate(
    abandono = ifelse(
      any(Trimestre == 1 & V3002 == "Sim") & !any(Trimestre == 2 & V3002 == "Sim"),
      1,
      0
    )
  ) %>%
  ungroup()

# Reorganizando as colunas para trazer as novas variáveis para o começo
base_abandono_filtrada <- base_abandono %>%
  select(
    id_individuo, Ano, Trimestre, faixa_idade_14_24, ensino_medio_dummie, residencia_unipessoal,
    rede_publica, renda_per_capta_menor_706, RDPC, região, educacao_mae, educacao_pai, abandono,
    everything()
  )

# Remover observações onde V20082 é igual a 9999
base_abandono_filtrada <- base_abandono_filtrada %>%
  filter(V20082 != 9999)

######################################## Tamanho do problema####################################################

## Definindo os 3 públicos


# Público potencial (14-24 anos, RDPC < 706)
publico_pontencial <- base_evasao_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24 & RDPC < 706
  )

# Público EM público (Rede pública, 14-24 anos)
em_publico <- base_evasao_filtrada %>%
  filter(
    V3002A == "Rede pública" & V2009 >= 14 & V2009 <= 24
  )

# Público alvo PDM (Rede pública, Ensino médio regular, RDPC < 706)
beneficiários_pdm <- base_evasao_filtrada %>%
  filter(
    V2009 >= 14 & V2009 <= 24 &          # Faixa etária de 14 a 24 anos
      V3002A == "Rede pública" &           # Escola pública
      V3003A == "Regular do ensino médio" & # Ensino médio regular
      RDPC < 706                           # Renda per capita menor que 706 reais
  )

######################## evasao por série ########################

# Calcular a evasao por série para o público potencial
evasao_publico_potencial <- publico_pontencial %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasao
  )

# Calcular a evasao por série para o público EM público
evasao_em_publico <- em_publico %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasao
  )

# Calcular a evasao por série para os beneficiários PDM
evasao_publico_alvo <- beneficiários_pdm %>%
  group_by(V3006) %>%  # Agrupar por série
  summarise(
    evasao_total = sum(evasao, na.rm = TRUE),  # Soma das evasões
    total_alunos = n(),                        # Total de alunos
    taxa_evasao = evasao_total / total_alunos  # Taxa de evasao
  )



############################################### Motivos do abandono###############################################


# Filtrando os dados para cada ano e selecionando apenas o primeiro trimestre
base_abandono_2022 <- base_abandono_filtrada %>%
  filter(Ano == 2022 & Trimestre == 1)

base_abandono_2023 <- base_abandono_filtrada %>%
  filter(Ano == 2023 & Trimestre == 1)

base_abandono_2024 <- base_abandono_filtrada %>%
  filter(Ano == 2024 & Trimestre == 1)

# Lista de variáveis de interesse
variaveis_independentes <- c(
  "V2010", "V2007", "V2009",  # Características demográficas
  "região",           # Região e situação do domicílio
  "educacao_mae", "educacao_pai", "RDPC",  # Educação e renda
  "V2001", "VD2004"           # Composição do domicílio
)

# Função para rodar o modelo probit
rodar_probit <- function(base, variaveis_independentes) {
  # Preparando as variáveis independentes e a variável dependente
  formula <- as.formula(paste("abandono ~", paste(variaveis_independentes, collapse = " + ")))
  modelo <- glm(formula, data = base, family = binomial(link = "probit"))
  summary(modelo)
}

# Rodando o modelo probit para cada ano
resumo_2022 <- rodar_probit(base_abandono_2022, variaveis_independentes)
resumo_2023 <- rodar_probit(base_abandono_2023, variaveis_independentes)

# Exibindo os resumos dos modelos
print(resumo_2022)
print(resumo_2023)


# Instalar e carregar o pacote stargazer, se ainda não estiver instalado
if (!require(stargazer)) {
  install.packages("stargazer")
  library(stargazer)
}

# y = aabandono
# x = sexo, raça, região


base_abandono_2023 <- base_abandono_2023 %>%
  mutate(
    regiao_norte = ifelse(substr(UPA, start = 1, stop = 1) == '1', 1, 0),
    regiao_nordeste = ifelse(substr(UPA, start = 1, stop = 1) == '2', 1, 0),
    regiao_sudeste = ifelse(substr(UPA, start = 1, stop = 1) == '3', 1, 0),
    regiao_sul = ifelse(substr(UPA, start = 1, stop = 1) == '4', 1, 0),
    regiao_centro_oeste = ifelse(substr(UPA, start = 1, stop = 1) == '5', 1, 0)
  )

base_abandono_2023 <- base_abandono_2023 %>%
  mutate(
    sexo_masculino = ifelse(V2007 == 1, 1, 0),  # Supondo que 1 representa masculino
    sexo_feminino = ifelse(V2007 == 2, 1, 0)    # Supondo que 2 representa feminino
  )


modelo_2023 <- glm(abandono ~ V2010 + V2009 + regiao_norte + regiao_nordeste + 
                     regiao_sudeste + regiao_sul + 
                     educacao_mae  + RDPC + V2001 + VD2004 + sexo_masculino,
                   data = base_abandono_2023, family = binomial(link = "probit"))

base_abandono_2022 <- base_abandono_2023 %>%
  mutate(
    sexo_masculino = ifelse(V2007 == 1, 1, 0),  # Supondo que 1 representa masculino
    sexo_feminino = ifelse(V2007 == 2, 1, 0)    # Supondo que 2 representa feminino
  )


base_abandono_2022 <- base_abandono_2023 %>%
  mutate(
    regiao_norte = ifelse(substr(UPA, start = 1, stop = 1) == '1', 1, 0),
    regiao_nordeste = ifelse(substr(UPA, start = 1, stop = 1) == '2', 1, 0),
    regiao_sudeste = ifelse(substr(UPA, start = 1, stop = 1) == '3', 1, 0),
    regiao_sul = ifelse(substr(UPA, start = 1, stop = 1) == '4', 1, 0),
    regiao_centro_oeste = ifelse(substr(UPA, start = 1, stop = 1) == '5', 1, 0)
  )

modelo_2022 <- glm(abandono ~ V2010 + V2009 + regiao_norte + regiao_nordeste + 
                     regiao_sudeste + regiao_sul + 
                     educacao_mae + + RDPC + V2001 + VD2004 + sexo_masculino,
                   data = base_abandono_2023, family = binomial(link = "probit"))


# Carregar o pacote
library(stargazer)

# Gerar tabela com stargazer
stargazer(
  modelo_2022, modelo_2023, 
  type = "text",           # Parxa exibir o resultado no console (mude para "latex" ou "html" conforme necessário)
  title = "Resultados dos Modelos Probit",
  dep.var.labels = c("Abandono"),  # Nome da variável dependente
  covariate.labels = c("Cor ou raça (V2010)", "Sexo feminino", "Idade (V2009)", 
                       "Região: Norte", "Região: Nordeste", "Região: Sudeste", "Região: Sul", "Região: Centro-Oeste", 
                       "Educação da mãe", "Educação do pai", "Renda per capita (RDPC)", 
                       "Número de pessoas no domicílio (V2001)", "Espécie da unidade doméstica (VD2004)"), 
  model.names = FALSE,      # Remover nomes dos modelos
  column.labels = c("2022", "2023"), # Colunas para os anos
  align = TRUE,             # Alinhar colunas
  no.space = TRUE,          # Remover espaço entre linhas
  star.cutoffs = c(0.1, 0.05, 0.01), # Níveis de significância
  digits = 3                # Número de casas decimais
)


# Gerar uma tabela visual com stargazer
tabela <- stargazer(modelo_2022, modelo_2023,
                    title = "Resultados dos Modelos Probit",
                    type = "text",  # Use "html" ou "latex" para outros formatos
                    out = "tabela_probit.html",  # Exporta a tabela em HTML (opcional)
                    dep.var.labels = "Abandono",
                    covariate.labels = c(
                      "Cor ou raça (V2010)", "Sexo (V2007)", "Idade (V2009)",
                      "Região", "Situação do domicílio (V1022)",
                      "Educação da mãe", "Educação do pai",
                      "Renda per capita (RDPC)",
                      "Número de pessoas no domicílio (V2001)", 
                      "Espécie da unidade doméstica (VD2004)"
                    ),
                    align = TRUE)


# Exemplo básico de tabela com o pacote gt
tabela_gt <- gt::gt(data.frame(summary(modelo_2022)$coefficients))
gt::gtsave(tabela_gt, "tabela_probit.png")  # Salva a tabela como imagem PNG

########## Variáveis de interesse ###########################################################################



1. #2010: Cor ou raça, #V2007: Sexo, #V2009: Idade do morador na data de referência

2. # região, #V1022: Situação do domicílio

3. # educacao_mae , educacao_pai , max educ pai/mãe, # RDPC : Rendimento domiciliar per capta

4 #V2001: Número de pessoas no domicílio,  #VD2004: Espécie da unidade doméstica

### Corrigir RDPC e educ mae e pai



write_dta(base_abandono_filtrada, "base_abandono_filtrada_teste.dta")






#VD3004: Nível de instrução mais elevado alcançado



# V3001: Sabe ler e escrever?


################################### Número evasao################################################################

# Verificar evasões em todas as transições de trimestre
evasoes_todos_periodos <- publico_alvo_filtrado %>%
  group_by(id_individuo) %>%
  summarise(
    # Verificar matrícula no 1º trimestre e ausência no 2º trimestre
    evasao_1_para_2 = any(Trimestre == "1" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "2" & V3003A == "Regular do ensino médio"),
    
    # Verificar matrícula no 2º trimestre e ausência no 3º trimestre
    evasao_2_para_3 = any(Trimestre == "2" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "3" & V3003A == "Regular do ensino médio"),
    
    # Verificar matrícula no 3º trimestre e ausência no 4º trimestre
    evasao_3_para_4 = any(Trimestre == "3" & V3003A == "Regular do ensino médio") & 
      !any(Trimestre == "4" & V3003A == "Regular do ensino médio")
  ) %>%
  # Filtrar indivíduos que tiveram evasao em qualquer um dos períodos
  filter(evasao_1_para_2 | evasao_2_para_3 | evasao_3_para_4) %>%
  ungroup()

# Verificar o número total de indivíduos que tiveram evasao em qualquer período
numero_total_de_evasoes <- nrow(evasoes_todos_periodos)
print(numero_total_de_evasoes)

###
library(dplyr)
library(plm)
library(fixest)
library(stargazer)
library(gt)

# Criação das variáveis de região e sexo sem redundâncias
base_abandono_2023 <- base_abandono_2023 %>%
  mutate(
    regiao = factor(substr(UPA, 1, 1), 
                    levels = c('1', '2', '3', '4', '5'),
                    labels = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste')),
    sexo = factor(V2007, levels = c(1, 2), labels = c('Masculino', 'Feminino'))
  )

# Modelo Probit - 2023
modelo_2023 <- glm(abandono ~ V2010 + V2009 + regiao + educacao_mae + 
                     educacao_pai + RDPC + V2001 + VD2004 + sexo,
                   data = base_abandono_2023, family = binomial(link = "probit"))

# Repetição para base de 2022 com as mesmas variáveis
base_abandono_2022 <- base_abandono_2023 %>%
  filter(Ano == 2022) %>%  # Filtrando para 2022, se necessário
  mutate(
    regiao = factor(substr(UPA, 1, 1), 
                    levels = c('1', '2', '3', '4', '5'),
                    labels = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste')),
    sexo = factor(V2007, levels = c(1, 2), labels = c('Masculino', 'Feminino'))
  )

# Modelo Probit - 2022
modelo_2022 <- glm(abandono ~ V2010 + V2009 + regiao + educacao_mae + 
                     educacao_pai + RDPC + V2001 + VD2004 + sexo,
                   data = base_abandono_2022, family = binomial(link = "probit"))

# Tabela dos resultados com stargazer
stargazer(modelo_2022, modelo_2023, 
          type = "text",
          title = "Resultados dos Modelos Probit",
          dep.var.labels = c("Abandono"),
          covariate.labels = c("Cor ou raça (V2010)", "Idade (V2009)", 
                               "Região: Norte", "Região: Nordeste", "Região: Sudeste", "Região: Sul", 
                               "Educação da mãe", "Educação do pai", "Renda per capita (RDPC)", 
                               "Número de pessoas no domicílio (V2001)", "Espécie da unidade doméstica (VD2004)",
                               "Sexo: Feminino"),
          column.labels = c("2022", "2023"),
          align = TRUE,
          no.space = TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)

# Tabela visual em gt
tabela_gt <- gt::gt(data.frame(summary(modelo_2022)$coefficients))
gt::gtsave(tabela_gt, "tabela_probit_2022.png")


###
# Verifique os níveis de cada variável categórica para garantir que elas tenham mais de um nível
sapply(base_abandono_2023, function(x) if (is.factor(x)) levels(x) else NULL)

# Verifique especificamente o número de níveis nas variáveis categóricas do modelo
cat_vars <- c("V2010", "regiao", "sexo")
lapply(base_abandono_2023[cat_vars], function(x) length(unique(x)))

###
install.packages("margins")
library(margins)

efeitos_marginais_2023 <- margins(modelo_2023)
summary(efeitos_marginais_2023)


# Gráfico dos efeitos marginais
plot(efeitos_marginais_2023, main = "Efeitos Marginais do Modelo 2023", xlab = "Idade", ylab = "Efeito Marginal")