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
# familia <- read_delim('base_amostra_familia_201812.csv', delim = ';', quote = '\'', show_col_types = FALSE)

# *Pessoas ####
pessoas <- read_delim('base_amostra_pessoa_201812.csv', delim = ';', quote = '\'', locale = locale(decimal_mark = ','), show_col_types = FALSE)

## 0.4 Renomear colunas ####
pessoas <- pessoas %>%
  rename_with(~ gsub('[\'\\\\]', '', .x))

#### 1. ANÁLISE EXPLORATÓRIA (AED) ####
### *PESSOAS ####
## 1.1 Sobre o df (pessoas) ###
head(pessoas)
str(pessoas)
names(pessoas)

## 1.2 Colunas ####
names(pessoas)

## 1.3 Descritiva ####
summary(pessoas)

#### 2. GRÁFICOS E TABELAS ####
## 2.1 Distribuição de Idades ####
pessoas %>%
  ggplot(aes(x = idade)) +
  geom_histogram(bins = 30, fill = 'skyblue', color = 'black') +
  labs(title = 'Distribuição de Idades no CadÚnico 2018', x = 'Idade', y = 'Frequência') +
  theme_minimal()

## 2.2 Distribuição de Gênero ####
pessoas %>%
  count(cod_sexo_pessoa) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(cod_sexo_pessoa), y = n, fill = factor(cod_sexo_pessoa))) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(perc, 1), '%')), vjust = -0.5, size = 3.5) +  # valores acima das barras
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  # y em milhões
  labs(title = 'Distribuição por Sexo', x = 'Sexo', y = 'Número de Pessoas') +
  theme_minimal() +
  theme(legend.position = 'none')

## 2.3 Escolaridade dos membros ####
pessoas %>%
  mutate(ind_frequenta_escola_memb = factor(ind_frequenta_escola_memb, 
                                            levels = c(1, 2, 3, 4), 
                                            labels = c('Sim', 'Não', 'Não informado', 'Outros'))) %>%
  count(ind_frequenta_escola_memb) %>%
  mutate(perc = (n / sum(n)) * 100) %>%  # Cálculo do percentual
  ggplot(aes(x = ind_frequenta_escola_memb, y = n, fill = ind_frequenta_escola_memb)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(perc, 1), '%')), 
            vjust = -0.5, 
            size = 3) +  # Exibe os valores percentuais acima das barras
  labs(title = 'Membros ainda se educam?', 
       x = 'Frequenta Escola', 
       y = 'Quantidade') +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  # Formato em milhões
  theme_minimal() +
  theme(legend.position = 'none')
unique(pessoas$ind_frequenta_escola_memb)

## 2.4 Renda média por faixa etária ####
pessoas %>%
  mutate(faixa_etaria = case_when(
    idade < 18 ~ '0-17',
    idade >= 18 & idade < 24 ~ '18-24',
    idade >= 24 & idade < 60 ~ '24-59',
    idade >= 60 ~ '60+'
  )) %>%
  group_by(faixa_etaria) %>%
  summarise(renda_media = mean(val_renda_bruta_12_meses_memb, na.rm = TRUE)) %>%
  gt() %>%
  tab_header(title = 'Renda Média por Faixa Etária no CadÚnico 2018') %>%
  cols_label(faixa_etaria = 'Faixa Etária', renda_media = 'Renda Média (R$)')

## 2.5 Distribuição de Cor/Raça ####
pessoas %>%
  mutate(cod_raca_cor_pessoa = factor(cod_raca_cor_pessoa, # Confirmar se levels estão corretos
                                      levels = 1:5, 
                                      labels = c('Preta', 'Branca', 'Amarela', 'Parda', 'Indígena'))) %>%
  count(cod_raca_cor_pessoa) %>%
  mutate(perc = (n / sum(n)) * 100) %>%  # Cálculo do percentual
  ggplot(aes(x = cod_raca_cor_pessoa, y = n, fill = cod_raca_cor_pessoa)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(perc, 1), '%')), 
            vjust = -0.5, 
            size = 3) +  # Exibe os valores percentuais acima das barras
  labs(title = 'Distribuição por Cor/Raça', 
       x = 'Cor/Raça', 
       y = 'Quantidade') +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = 'M')) +  # Formato em milhões
  theme_minimal() +
  theme(legend.position = 'none')

## 2.6 Tipos de remuneração ####
pessoas %>%
  summarise(
    media_remuneracao = mean(val_remuner_emprego_memb, na.rm = TRUE),
    mediana_remuneracao = median(val_remuner_emprego_memb, na.rm = TRUE),
    media_outros_rendimentos = mean(val_outras_rendas_memb, na.rm = TRUE),
    mediana_outros_rendimentos = median(val_outras_rendas_memb, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(title = 'Estatísticas de Remuneração e Outras Rendas no CadÚnico 2018') %>%
  cols_label(
    media_remuneracao = 'Média da Remuneração Formal (R$)',
    mediana_remuneracao = 'Mediana da Remuneração Formal (R$)',
    media_outros_rendimentos = 'Média de Outros Rendimentos (R$)',
    mediana_outros_rendimentos = 'Mediana de Outros Rendimentos (R$)'
  )
