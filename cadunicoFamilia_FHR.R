## DADOS CADÚNICO (2018) -- Análise exploratória
# Autores: Fernando, Helena e Rafaela (FHR)

#### 0. CONFIGURAR AMBIENTE E DF ####
## 0.1 Local de trabalho ####
# (!) MUDAR (se for o caso)
local <- 'C:\\Users\\ferna\\OneDrive\\Desktop OneDrive\\base_amostra_cad_201812\\base_amostra_cad_201812'
setwd(local)

## 0.2 Bibliotecas ####
library(PNADcIBGE) # Dados PNADc
library(survey) # Estratificação e Clusterização de dados
library(convey) # Cálculo de medidas de desigualdade
library(tidyverse) # Manipulação do df e Gráficos
library(gt)  # Criar Tabelas
library(scales)  # Formatação de gráficos

## 0.3 Carregar dfs ####
# *Família ####
familia <- read_delim('base_amostra_familia_201812.csv', delim = ';', quote = '\'', show_col_types = FALSE)
familia <- familia %>%
  filter(qtde_pessoas >= 14 & qtde_pessoas <= 24)
# *Pessoas ####
# pessoas <- read_delim('base_amostra_pessoa_201812.csv', delim = ';', quote = '\'', locale = locale(decimal_mark = ','), show_col_types = FALSE)

## 0.4 Renomear colunas ####
names(familia) <- gsub("\"", "", names(familia))


#### 1. ANÁLISE EXPLORATÓRIA (AED) ####
### *FAMILIA ####
## 1.1 Sobre o df (familia) ###
head(familia)
str(familia)
names(familia)

## 1.2 Colunas ####
names(familia)

## 1.3 Descritiva ####
summary(familia)

#### 2. GRÁFICOS E TABELAS ####
## 2.1 Estrutura geral dos dados ####
summary_table <- familia %>%
  summarise(
    Total_Observacoes = n(),
    Renda_Media = mean(vlr_renda_media_fam, na.rm = TRUE),
    Pessoas_Media = mean(qtde_pessoas, na.rm = TRUE),
    Comodos_Media = mean(qtd_comodos_domic_fam, na.rm = TRUE),
    Perc_Fam_Indigenas = mean(cod_familia_indigena_fam == 1, na.rm = TRUE) * 100,
    Perc_Fam_Quilombolas = mean(ind_familia_quilombola_fam == 1, na.rm = TRUE) * 100
  )

summary_table %>%
  gt() %>%
  tab_header(title = 'Resumo Geral das Famílias do CadÚnico 2018')

## 2.2 Renda Familiar Média ####
# *Escala Linear ####
familia %>%
  filter(!is.na(vlr_renda_media_fam), vlr_renda_media_fam > 0) %>%
  ggplot(aes(y = vlr_renda_media_fam)) +
  geom_boxplot(fill = 'coral') +
  labs(title = 'Distribuição da Renda Familiar Média (Escala Linear)', y = 'Renda Familiar Média') +
  theme_minimal()

# *Escala Logarítmica ####
familia %>%
  filter(!is.na(vlr_renda_media_fam), vlr_renda_media_fam > 0) %>%
  ggplot(aes(y = vlr_renda_media_fam)) +
  geom_boxplot(fill = 'coral') +
  scale_y_log10() +
  labs(title = 'Distribuição da Renda Familiar Média (Escala Logarítmica)', y = 'Renda Familiar Média (Log)') +
  theme_minimal()

# *Escala Linear até R$5k ####
# ~3 salários mínimos
familia %>%
  filter(!is.na(vlr_renda_media_fam), vlr_renda_media_fam > 0, vlr_renda_media_fam <= 5000) %>%
  ggplot(aes(y = vlr_renda_media_fam)) +
  geom_boxplot(fill = 'coral') +
  labs(title = 'Distribuição da Renda Familiar Média (Até R$5.000)', y = 'Renda Familiar Média') +
  theme_minimal()

## 2.3 Quantidade de Pessoas por Família ####
familia %>%
  filter(!is.na(qtde_pessoas)) %>%
  ggplot(aes(x = qtde_pessoas)) +
  geom_histogram(binwidth = 1, fill = 'skyblue', color = 'white') +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  # Eixo Y em milhões
  scale_x_continuous(limits = c(0, 35)) +  # Limita o eixo X a 35 pessoas
  labs(title = 'Distribuição da Quantidade de Pessoas por Família', x = 'Quantidade de Pessoas', y = 'Frequência (Milhões)') +
  theme_minimal()
summary(familia$qtde_pessoas)
table(familia$qtde_pessoas)
round(prop.table(table(familia$qtde_pessoas)) * 100, 2)

## 2.4 Condições do Domicílio ####
familia %>%
  filter(!is.na(cod_agua_canalizada_fam)) %>%
  count(cod_agua_canalizada_fam) %>%
  mutate(
    total = sum(n),
    relative = (n / total) * 100,  # Calcula o percentual
    label = paste0(round(relative, 1), '%')  # Formata o rótulo para exibir como percentual
  ) %>%
  ggplot(aes(x = factor(cod_agua_canalizada_fam), y = n, fill = factor(cod_agua_canalizada_fam))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +  # Adiciona os valores relativos acima das barras
  scale_fill_manual(values = c('darkgreen', 'lightgreen'), labels = c('Sem água canalizada', 'Com água canalizada')) +
  scale_x_discrete(labels = c('1' = 'Sim', '2' = 'Não')) +  # Substitui 1 e 2 por Sim e Não
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  # Formata o eixo Y em milhões
  labs(title = 'Acesso à Água Canalizada', x = 'Água Canalizada', y = 'Frequência (Milhões)') +
  theme_minimal() +
  theme(legend.position = 'none')


familia %>%
  filter(!is.na(qtd_comodos_domic_fam), qtd_comodos_domic_fam <= 10) %>%
  ggplot(aes(x = qtd_comodos_domic_fam)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição da Quantidade de Cômodos nos Domicílios", x = "Quantidade de Cômodos", y = "Frequência") +
  theme_minimal()

## 2.5 Renda média da família por Estado (UF) ####
# Mapeamento dos códigos IBGE para as siglas dos estados
siglas_estados <- c(
  '11' = 'RO', '12' = 'AC', '13' = 'AM', '14' = 'RR', '15' = 'PA', '16' = 'AP', '17' = 'TO',
  '21' = 'MA', '22' = 'PI', '23' = 'CE', '24' = 'RN', '25' = 'PB', '26' = 'PE', '27' = 'AL',
  '28' = 'SE', '29' = 'BA', '31' = 'MG', '32' = 'ES', '33' = 'RJ', '35' = 'SP', '41' = 'PR',
  '42' = 'SC', '43' = 'RS', '50' = 'MS', '51' = 'MT', '52' = 'GO', '53' = 'DF'
)

# Adiciona a coluna de siglas de estado
familia <- familia %>%
  mutate(estado = trunc(cd_ibge / 100000),
         sigla_estado = siglas_estados[as.character(estado)])

# Calcule a renda média por sigla do estado
renda_media_por_estado <- familia %>%
  group_by(sigla_estado) %>%
  summarise(renda_media = mean(vlr_renda_media_fam, na.rm = TRUE)) %>%
  arrange(sigla_estado)

# Crie o gráfico de barras da renda média por estado
renda_media_por_estado %>%
  ggplot(aes(x = sigla_estado, y = renda_media)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Estado', y = 'Renda Média (R$)', title = 'Renda Média por Estado') +
  theme_minimal()

## 2.6 Programas Sociais ####
familia %>%
  count(marc_pbf) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(marc_pbf), y = perc, fill = factor(marc_pbf))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('gray', 'blue'), labels = c('Não Recebe', 'Recebe')) +
  scale_x_discrete(labels = c('0' = 'Não', '1' = 'Sim')) +
  labs(title = 'Participação no Programa Bolsa Família', x = 'Bolsa Família', y = 'Percentual') +
  theme_minimal() +
  theme(legend.position = 'none')

# cadunico
# 14 a 24 anos
# bolsa familia
# dummy: elegíveis e potenciais
# público alvo por raça

