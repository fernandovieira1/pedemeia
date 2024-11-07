## DADOS CADÚNICO (2018) -- Análise exploratória
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

## 0.3 Carregar dfs ####
# *Família ####
# familia <- read_delim('base_amostra_familia_201812.csv', delim = ';', quote = '\'', show_col_types = FALSE)

# *Pessoas ####
pessoas <- read_delim('base_amostra_pessoa_201812.csv', delim = ';', quote = '\'', locale = locale(decimal_mark = ','), show_col_types = FALSE)

## 0.4 Renomear colunas ####
names(pessoas) <- gsub('"', '', names(pessoas))

## 0.5 Filtrar público alvo ####
pessoas <- pessoas %>%
  filter(idade >= 14 & idade <= 24)

#### 1. ANÁLISE EXPLORATÓRIA (AED) ####
### *PESSOAS ####
## 1.1 Sobre o df (pessoas) ###
head(pessoas)
str(pessoas)

## 1.2 Colunas ####
names(pessoas)

## 1.3 Descritiva ####
summary(pessoas)

## 1.4 Descrição das colunas ####

# cd_ibge: Código IBGE do município onde a família está registrada.
# estrato: Estrato social ao qual a família pertence, categorizando em faixas de renda.
# classf: Classificação socioeconômica da família.
# id_familia: Identificador único para cada família.
# id_pessoa: Identificador único para cada pessoa dentro da família.
# cod_sexo_pessoa: Código representando o sexo da pessoa (masculino/feminino).
# idade: Idade da pessoa em anos.
# cod_parentesco_rf_pessoa: Código de parentesco da pessoa em relação ao responsável familiar.
# cod_raca_cor_pessoa: Código de raça/cor declarada da pessoa.
# cod_local_nascimento_pessoa: Código do local de nascimento da pessoa (cidade, estado).
# cod_certidao_registrada_pessoa: Indicador se a pessoa possui certidão de nascimento registrada.
# cod_deficiencia_memb: Código indicando se a pessoa tem algum tipo de deficiência.
# cod_sabe_ler_escrever_memb: Código indicando se a pessoa sabe ler e escrever.
# ind_frequenta_escola_memb: Indicador se a pessoa está matriculada e frequenta escola.
# cod_escola_local_memb: Código representando o tipo de escola (pública/privada) que frequenta.
# cod_curso_frequenta_memb: Código do curso ou série em que a pessoa está matriculada.
# cod_ano_serie_frequenta_memb: Código do ano ou série escolar que a pessoa está cursando.
# cod_curso_frequentou_pessoa_memb: Código do curso ou série mais avançada frequentada pela pessoa.
# cod_ano_serie_frequentou_memb: Código do ano ou série escolar mais avançada completada pela pessoa.
# cod_concluiu_frequentou_memb: Código indicando se concluiu o curso ou série escolar mais avançada frequentada.
# cod_trabalhou_memb: Código indicando se a pessoa trabalhou recentemente.
# cod_afastado_trab_memb: Código indicando se a pessoa estava afastada do trabalho.
# cod_agricultura_trab_memb: Código indicando se a pessoa trabalhou na agricultura.
# cod_principal_trab_memb: Código representando a ocupação principal da pessoa.
# val_remuner_emprego_memb: Valor da remuneração do emprego principal da pessoa.
# cod_trabalho_12_meses_memb: Código indicando se a pessoa trabalhou nos últimos 12 meses.
# qtd_meses_12_meses_memb: Quantidade de meses trabalhados nos últimos 12 meses pela pessoa.
# val_renda_bruta_12_meses_memb: Valor da renda bruta total nos últimos 12 meses.
# val_renda_doacao_memb: Valor da renda obtida por doações nos últimos 12 meses.
# val_renda_aposent_memb: Valor da renda proveniente de aposentadoria.
# val_renda_seguro_desemp_memb: Valor da renda proveniente do seguro-desemprego.
# val_renda_pensao_alimen_memb: Valor da renda proveniente de pensão alimentícia.
# val_outras_rendas_memb: Valor de outras fontes de renda não especificadas.
# peso.fam: Peso amostral da família (para estimativas representativas).
# peso.pes: Peso amostral da pessoa (para estimativas representativas).

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
