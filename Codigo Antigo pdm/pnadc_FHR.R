## DADOS PNAD (2018) -- Análise exploratória
# Autores: Fernando, Helena e Rafaela (FHR)

#### 0. CONFIGURAR AMBIENTE E DF ####
## 0.1 Local de trabalho ####
# (!) MUDAR (se for o caso)
local <- 'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Desafio Eco II - Pe de Meia\\BDs Pe de Meia'
setwd(local)

## 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

## 0.3 Carregar df pnad ####
# De cada trimestre
# Definir ano e trimestres desejados
ano <- 2023
trimestres <- c(1, 2, 3, 4)  # Escolha os trim que deseja, ex: c(1, 2) para apenas t1 e t2

# Variáveis a serem selecionadas
variaveis <- c(
  # Parte 1 - Identificação e Controle
  'UF',      # Unidade da Federação
  'V2001',   # Tamanho da família
  'V1022',   # Estrutura familiar (pai, mãe, monoparental)
  'V1023',   # Localização geográfica (rural ou urbana)
  
  # Parte 2 - Características do domicílio
  'V2001',   # Tamanho da família
  'V2003',   # Ordem do morador na família
  'V2005',   # Condição do domicílio
  'V2007',   # Sexo do morador
  'V20081',  # Mês de nascimento do morador
  'V20082',  # Ano de nascimento do morador
  'V2009',   # Idade do morador
  'V2010',   # Cor ou raça do morador
  
  # Parte 3 - Educação
  'VD3005',  # Educação da mãe
  'VD3006',  # Educação do pai
  'V3003A',  # Curso que morador frequenta
  'V3009A',  # Maior escolaridade atual do morador
  
  # Parte 4 - Trabalho
  'V4001',   # Trabalhou na semana de referência?
  'VD4019'   # Rendimento domiciliar per capita
)

# Lista para armazenar dfs trim
pnad_list <- list()

# Loop para carregar e armazenar os dados de cada trim
for (t in trimestres) {
  # Carregar dados para o trimestre `t`
  pnad_data <- get_pnadc(year = ano, quarter = t, vars = variaveis)
  
  # Criar o nome da variável dinamicamente (pnad_q1, pnad_q2, etc.)
  assign(paste0("pnad_q", t), pnad_data$variables)
  
  # Adicionar o df à lista
  pnad_list[[paste0("pnad_q", t)]] <- pnad_data$variables
}

# Juntar os dfs selecionados
pnad <- bind_rows(pnad_list)
gc()

#### 1. ANÁLISE EXPLORATÓRIA (AED) ####
## 1.1 Sobre o df (pnad) ###
head(pnad)
str(pnad)
names(pnad)

## 1.2 Colunas pnad ####
# Parte 1 - Identificação e Controle
# UF: Unidade da Federação
# V2001:      Tamanho da família
# V1022:      Estrutura familiar (pai, mãe, monoparental)
# V1023:      Localização geográfica (rural ou urbana)
# 
# # Parte 2 - Características do domicílio
# V2001:      Tamanho da família
# V2003:      Ordem do morador na família
# V2005:      Condição do domicílio
# V2007:      Sexo do morador
# V20081:     Mês de nascimento do morador
# V20082:     Ano de nascimento do morador
# V2009:      Idade do morador
# V2010:      Cor ou raça do morador
# 
# # Parte 3 - Educação
# VD3005:     Educação da mãe
# VD3006:     Educação do pai
# V3003A:     Curso que morador frequenta
# V3009A:     Maior escolaridade atual do morador
# 
# # Parte 4 - Trabalho
# V4001:      Trabalhou na semana de referência?
# VD4019:     Rendimento domiciliar per capita

## 1.3 Descritiva ####
summary(pnad)

#### 2. GRÁFICOS E TABELAS ####
## 2.1 Estrutura geral dos dados ####
summary_table <- pnad %>%
  summarise(
    Total_Observacoes = n(),
    Tamanho_Familia_Media = mean(V2001, na.rm = TRUE),
    Idade_Media = mean(V2009, na.rm = TRUE),
    Renda_Per_Capita_Media = mean(VD4019, na.rm = TRUE),
    `Distribuição Sexo` = paste0(round(prop.table(table(V2007)) * 100, 1), collapse = ', '),
    `Distribuição Localização` = paste0(round(prop.table(table(V1023)) * 100, 1), collapse = ', ')
  )

# Exibir a tabela com gt
summary_table %>%
  gt() %>%
  tab_header(title = 'Resumo Geral dos Dados')

# *Interpretação ####
# Sexo
table(pnad$V2007)

# Localização
table(pnad$V1023)

## 2.2  Idade e Sexo dos Moradores ####
# *Idade ####
ggplot(pnad, aes(x = V2009)) +
  geom_histogram(binwidth = 5, fill = 'skyblue', color = 'white') +
  labs(title = 'Distribuição da Idade dos Moradores', x = 'Idade', y = 'Frequência')

# *Sexo ####
ggplot(pnad, aes(x = factor(V2007), fill = factor(V2007))) +
  geom_bar() +
  scale_fill_manual(values = c('lightblue', 'lightpink'), labels = c('Masculino', 'Feminino')) +
  labs(title = 'Distribuição de Sexo', x = 'Sexo', y = 'Frequência') +
  theme(legend.position = 'none')

## 2.3 Localização Geográfica (Rural vs. Urbana) ####
pnad %>%
  count(V1023) %>%
  mutate(
    total = sum(n),
    relative = n / total * 100,
    label = ifelse(n >= 1e6, 
                   paste0(label_number(scale = 1e-6, suffix = 'M')(n), ' (', round(relative, 1), '%)'),
                   paste0(label_number(scale = 1e-3, suffix = ' mil')(n), ' (', round(relative, 1), '%)'))
  ) %>%
  ggplot(aes(x = factor(V1023), y = n, fill = factor(V1023))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c('darkgreen', 'lightgreen', 'yellow', 'orange'), 
                    labels = c('Capital', 'Resto da RM', 'Resto da RIDE', 'Resto da UF')) +
  scale_x_discrete(labels = function(x) gsub('\\s*\\([^\\)]+\\)', '', x)) +  
  scale_y_continuous(labels = NULL) +  # Remove o eixo Y
  labs(title = 'Distribuição por Localização Geográfica', x = 'Localização') +
  theme_minimal()
unique(pnad$V1023)

## 2.3 Nível de Escolaridade dos Moradores ####
pnad %>%
  count(V3009A) %>%
  mutate(
    total = sum(n),
    relative = (n / total) * 100,
    label = paste0(round(relative, 1), '%')
  ) %>%
  ggplot(aes(x = factor(V3009A), y = n, fill = factor(V3009A))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +  
  scale_fill_manual(values = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854', '#ffd92f', 
                               '#e5c494', '#b3b3b3', '#1b9e77', '#d95f02', '#7570b3', '#e7298a',
                               '#66a61e', '#e6ab02', '#a6761d', '#666666')) +  
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  
  labs(title = 'Distribuição de Escolaridade', x = 'Escolaridade', y = 'Frequência') +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)  # Rotaciona e ajusta o tamanho do texto do eixo X
  )

pnad %>%
  count(VD3005) %>%
  mutate(
    total = sum(n),
    relative = (n / total) * 100,
    label = paste0(round(relative, 1), '%')
  ) %>%
  ggplot(aes(x = factor(VD3005), y = n, fill = factor(VD3005))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +  
  scale_fill_manual(values = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', '#a6d854', '#ffd92f', 
                               '#e5c494', '#b3b3b3', '#1b9e77', '#d95f02', '#7570b3', '#e7298a',
                               '#66a61e', '#e6ab02', '#a6761d', '#666666')) +  
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  
  labs(title = 'Distribuição de Escolaridade', x = 'Escolaridade', y = 'Frequência') +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)  # Rotaciona e ajusta o tamanho do texto do eixo X
  )

## 2.4 Renda Domiciliar Per Capita ####
# *Escala Linear ####
pnad %>%
  filter(!is.na(VD4019)) %>%  # Remove valores NA de VD4019
  ggplot(aes(y = VD4019)) +
  geom_boxplot(fill = 'coral') +
  labs(title = 'Distribuição de Renda Per Capita', y = 'Renda Per Capita') +
  theme_minimal()
# Muitos outliers

# *Escala Logarítmica ####
pnad %>%
  filter(!is.na(VD4019), VD4019 > 0) %>%  # Remove valores NA e valores não positivos
  ggplot(aes(y = VD4019)) +
  geom_boxplot(fill = 'coral') +
  scale_y_log10() +  # Escala logarítmica no eixo Y
  labs(title = 'Distribuição de Renda Per Capita (Escala Logarítmica)', y = 'Renda Per Capita (Log)') +
  theme_minimal()

# *Escala Linear até R$5k ####
# ~3 salários mínimos
pnad %>%
  filter(!is.na(VD4019), VD4019 > 0, VD4019 <= 5000) %>%  # Filtra valores até 10.000
  ggplot(aes(y = VD4019)) +
  geom_boxplot(fill = 'coral') +
  labs(title = 'Distribuição de Renda Per Capita (Até R$10.000)', y = 'Renda Per Capita') +
  theme_minimal()

## 2.5 Tamanho Médio da Família por Estado (UF) ####
# Tamanho médio da família por estado
tamanho_familia_uf <- pnad %>%
  group_by(UF) %>%
  summarise(Tamanho_Familia_Medio = mean(V2001, na.rm = TRUE))

tamanho_familia_uf %>%
  gt() %>%
  tab_header(title = 'Tamanho Médio da Família por Estado')

## 2.6 Renda Per Capita por Estado (UF) ####
renda_per_capita_uf <- pnad %>%
  group_by(UF) %>%
  summarise(Renda_Per_Capita_Media = mean(VD4019, na.rm = TRUE))

ggplot(renda_per_capita_uf, aes(x = reorder(UF, -Renda_Per_Capita_Media), y = Renda_Per_Capita_Media)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Renda Per Capita Média por Estado", x = "Estado (UF)", y = "Renda Per Capita Média") +
  theme_minimal() +
  coord_flip()

