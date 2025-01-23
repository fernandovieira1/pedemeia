### GRÁFICOS E TABELAS (COM PESOS)
# - Preparação da versão final, aplicando os pesos para a população e formatando o código tex.
gc(); cat('\014')

## | ####


## TABELAS E GRÁFICOS TEXTO ####


## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.2.1A Resumo Descritivo da Cor/Raça ####

## i) Objeto da análise preliminar (amostral) ####
htmltools::html_print(a_tab_resumo_cor)

origem <- '%1.2.1A Resumo Descritivo da Cor/Raça'

## ii) Pesos ####
# Desenho amostral
desenho <- svydesign(
  ids = ~UPA,        
  strata = ~Estrato,    
  weights = ~V1028032,   
  data = base_evasao_filtrada,
  nest = TRUE           
)

## iii) Ajustar os cálculos ponderados ####
tabela_cor_raca_ponderada <- svytable(~V2010 + evasao, design = desenho)
proporcao_ponderada <- prop.table(tabela_cor_raca_ponderada, margin = 1) * 100

## iv) Converter em um data.frame para visualização ####
tabela_cor_raca_ponderada_df <- as.data.frame(as.table(proporcao_ponderada))
colnames(tabela_cor_raca_ponderada_df) <- c('V2010', 'evasao', 'Proporcao')

## v) Código original ####
# Resumo estatístico: Contagem e proporção de evasão por cor/raça
tabela_cor_raca <- tabela_cor_raca_ponderada_df %>%
  mutate(Proporcao = round(Proporcao, 2)) %>% 
  as.data.frame() 

# Filtrar apenas os índices de evasão
tabela_cor_raca <- tabela_cor_raca %>% 
  filter(evasao == 1) %>% 
  filter(V2010 != 'Ignorado') %>%
  arrange(desc(Proporcao)) %>% 
  select(-evasao)

# Renomear as colunas para evitar problemas com nomes vazios ou inválidos
colnames(tabela_cor_raca) <- c('Cor', '%')

# Formatar %
tabela_cor_raca <- tabela_cor_raca %>%
  mutate(`%` = paste0(format(`%`, nsmall = 2, decimal.mark = ',')))
tabela_cor_raca

# Gerar o título dinâmico com as variáveis 'inicio' e 'fim' sem aspas
titulo_dinamico <- paste('Evasão Escolar Cor', paste0('Período: ', inicio, '-', fim), sep = '\n')


# Exibir a tabela com stargazer (em latex)
a_tab_resumo_cor_latex <- stargazer(
  tabela_cor_raca,
  type = 'latex',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

## vi) Formatação final ####
# Remover as linhas de comentário geradas pelo stargazer
a_tab_resumo_cor_latex <- gsub('^%.*$', '', a_tab_resumo_cor_latex)

# Adicionar uma formatação com indentação para exibição organizada
a_tab_resumo_cor_latex <- paste(a_tab_resumo_cor_latex, collapse = '\n')

## vii) Exibir o resultado ####
a_tab_resumo_cor_latex <- paste0(
  origem,
  a_tab_resumo_cor_latex
)
cat(a_tab_resumo_cor_latex)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.2.3A Gráfico com Percentuais no Topo ####

## i) Objeto da análise preliminar (amostral) ####
# Objeto da análise preliminar (amostral)
a_graf_cor_percentual

origem <- '%1.2.3A Gráfico com Percentuais no Topo'

## ii) Pesos ####

# Garantir que 'V2010' e 'evasao' são fatores
base_evasao_filtrada$V2010 <- as.factor(base_evasao_filtrada$V2010)
base_evasao_filtrada$evasao <- as.factor(base_evasao_filtrada$evasao)
base_evasao_filtrada <- base_evasao_filtrada[base_evasao_filtrada$V1028032 > 0, ]


# Desenho amostral
desenho <- svydesign(
  ids = ~UPA,        
  strata = ~Estrato,    
  weights = ~V1028032,   
  data = base_evasao_filtrada,
  nest = TRUE           
)

# Calcular os totais ponderados (população estimada) por cor e evasão
totais_ponderados <- svytable(~V2010 + evasao, design = desenho) # Usar svytable para corrigir o erro no svyby

# Transformar os resultados em data.frame
totais_ponderados_df <- as.data.frame(totais_ponderados)
colnames(totais_ponderados_df) <- c('Cor', 'evasao', 'Populacao')

## iii) Ajustar os cálculos ponderados ####
base_evasao_percentual_cor_ponderada <- prop.table(totais_ponderados, margin = 1) * 100

## iv) Converter em um data.frame para visualização ####
base_evasao_percentual_cor_ponderada_df <- as.data.frame(as.table(base_evasao_percentual_cor_ponderada))
colnames(base_evasao_percentual_cor_ponderada_df) <- c('Cor', 'evasao', 'Percentual')

# Combinar com o data frame de proporções
base_evasao_percentual_cor_ponderada_df <- merge(
  base_evasao_percentual_cor_ponderada_df, 
  totais_ponderados_df, 
  by = c('Cor', 'evasao')
)

## v) Código original ####
# --> Comparar com as variáveis de ii) e iii)

# Calcular os percentuais de evasão por cor/raça
base_evasao_percentual_cor_ponderada_df <- base_evasao_percentual_cor_ponderada_df %>%
  mutate(
    Percentual = round(Percentual, 2),
    Populacao = format(
      round(Populacao, 0),  # Dividir por 1.000 e arredondar para duas casas decimais
      big.mark = '.',            # Ponto como separador de milhar
      decimal.mark = ',',        # Vírgula como separador decimal
      scientific = FALSE         # Evitar notação científica
    )
  ) %>%
  as.data.frame()

# Visualizar o resultado
print(base_evasao_percentual_cor_ponderada_df)


# Gráfico com os percentuais no topo das barras
# Primeiro gráfico: Categorias individuais
grafico_percentual <- base_evasao_percentual_cor_ponderada_df %>%
  filter(evasao == 1) %>%
  filter(Cor != 'Ignorado') %>%
  ggplot(aes(x = Cor, y = Percentual, fill = Cor)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0('(', Percentual, '%)')),  # Exibe os percentuais
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  scale_fill_manual(
    values = c('Branca' = 'lightblue', 'Preta' = '#5586e3', 
               'Parda' = '#5586e3', 'Amarela' = '#5586e3', 
               'Indígena' = '#5586e3')
  ) +
  labs(
    title = paste0('Evasão por Cor - Categorias Individuais - Período ', inicio, '-', fim),
    y = 'Percentual (%)'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = 'none'
  )

# Segundo gráfico: Branca vs Não-Brancos
grafico_agrupado <- base_evasao_percentual_cor_ponderada_df %>%
  mutate(
    Cor = ifelse(Cor == 'Branca', 'Branca', 'Não-Brancos')  # Reclassificar as categorias
  ) %>%
  group_by(Cor, evasao) %>%
  summarise(
    Populacao = sum(as.numeric(gsub('\\.', '', Populacao))),  # Somar as populações reclassificadas
    .groups = 'drop'
  ) %>%
  group_by(Cor) %>%
  summarise(
    Percentual = sum(Populacao[evasao == 1]) / sum(Populacao) * 100,  # Cálculo correto do percentual
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Cor, y = Percentual, fill = Cor)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0('(', round(Percentual, 2), '%)')), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  scale_fill_manual(
    values = c('Branca' = 'lightblue', 'Não-Brancos' = '#5586e3')
  ) +
  labs(
    title = paste0('Evasão por Cor - Branca vs. Não-Brancos - Período ', inicio, '-', fim)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),  # Remove a descrição do eixo X
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'none'
  )

## vi) Resultado ####
# Combinar os gráficos em uma grade
grid.arrange(grafico_percentual, grafico_agrupado, ncol = 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.3.3A Gráfico com Percentuais no Topo ####
## i) Objeto da análise preliminar (amostral)
a_graf_sexo_percentual

origem <- '%1.3.3A Gráfico com Percentuais no Topo'

## ii) Pesos ####

# Garantir que 'V2007' e 'evasao' são fatores
base_evasao_filtrada$V2007 <- as.factor(base_evasao_filtrada$V2007)
base_evasao_filtrada$evasao <- as.factor(base_evasao_filtrada$evasao)
base_evasao_filtrada <- base_evasao_filtrada[base_evasao_filtrada$V1028032 > 0, ]

# Desenho amostral
desenho <- svydesign(
  ids = ~UPA,        
  strata = ~Estrato,    
  weights = ~V1028032,   
  data = base_evasao_filtrada,
  nest = TRUE           
)

# Calcular os totais ponderados
totais_ponderados <- svytable(~V2007 + evasao, design = desenho) 

# Transformar os resultados em data.frame
totais_ponderados_df <- as.data.frame(totais_ponderados)
colnames(totais_ponderados_df) <- c('Sexo', 'evasao', 'Populacao')

## iii) Ajustar os cálculos ponderados ####
base_evasao_percentual_sexo_ponderada <- prop.table(totais_ponderados, margin = 1) * 100

## iv) Converter em um data.frame para visualização ####
base_evasao_percentual_sexo_ponderada_df <- as.data.frame(as.table(base_evasao_percentual_sexo_ponderada))
colnames(base_evasao_percentual_sexo_ponderada_df) <- c('Sexo', 'evasao', 'Populacao')

# Combinar com o data frame de proporções
base_evasao_percentual_sexo_ponderada_df <- merge(
  base_evasao_percentual_sexo_ponderada_df, 
  totais_ponderados_df, 
  by = c('Sexo', 'evasao')
)

## v) Código original ####
# --> Comparar com as variáveis de ii) e iii)
# Obter os valores mínimo e máximo da variável 'anos'
inicio <- min(anos)
fim <- max(anos)

# Calcular os percentuais de evasão por sexo
base_evasao_percentual_sexo_peso <- base_evasao_filtrada %>%
  group_by(V2007, evasao) %>%
  summarise(Contagem = n(), .groups = 'drop') %>%
  group_by(V2007) %>%
  mutate(Percentual = round(Contagem / sum(Contagem) * 100, 2))

base_evasao_percentual_sexo_peso %>%
  filter(evasao == 1)

# Gráfico com os percentuais no topo das barras
base_evasao_percentual_sexo_peso %>%
  filter(evasao == 1) %>%
  ggplot(aes(x = V2007, y = Contagem, fill = V2007)) + # Alterando o 'fill' para mapear 'Homem' e 'Mulher'
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(aes(label = paste0(Percentual, '%')), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = paste0('Evasão por Sexo: Período ', inicio, '-', fim),
       x = 'Sexo',
       y = 'Frequência',
       fill = 'Sexo') + # Alterando o rótulo da legenda para 'Sexo'
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = 'none'
  ) +
  scale_fill_manual(
    values = c('Homem' = 'bisque4', 'Mulher' = 'bisque2')
  ) -> a_graf_sexo_percentual_peso
a_graf_sexo_percentual_peso

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## 1.7.1A Resumo Descritivo do RDPC por Cor ####

## i) Objeto da análise preliminar (amostral) ####
htmltools::html_print(a_tab_resumo_rdpc_cor)

origem <- '1.7.1A Resumo Descritivo do RDPC por Cor'

# Adicionar o salário mínimo à base, calculado para cada ano
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(Salario_Minimo = sal_min(Ano))  # Adiciona o salário mínimo correspondente ao ano

# Filtrar para garantir que não há valores NA ou zero em Salario_Minimo, RDPC e V2010 (cor)
base_evasao_filtrada <- base_evasao_filtrada %>%
  filter(!is.na(Salario_Minimo) & !is.na(RDPC) & Salario_Minimo > 0 & !is.na(V2010))

# Diagnóstico para verificar valores fora dos limites dos breaks
ajuste <- 1e-4  # Ajuste para evitar problemas de precisão numérica

# Criar categorias de RDPC por cor
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(
    Faixa_RDPC = case_when(
      RDPC <= 0.5 * Salario_Minimo ~ 'Até 0.5 SM',
      RDPC <= Salario_Minimo ~ '0.5 a 1 SM',
      RDPC <= 2 * Salario_Minimo ~ '1 a 2 SM',
      RDPC <= 5 * Salario_Minimo ~ '2 a 5 SM',
      RDPC <= 10 * Salario_Minimo ~ '5 a 10 SM',
      RDPC <= 20 * Salario_Minimo ~ '10 a 20 SM',
      TRUE ~ 'Acima de 20 SM'
    )
  ) 

# Remover cor 'Ignorado'
base_evasao_filtrada %>%
  filter(V2010 != 'Ignorado') -> base_evasao_filtrada

# Reduzir V2010 a brancos e não-brancos
base_evasao_filtrada <- base_evasao_filtrada %>%
  mutate(V2010 = ifelse(V2010 == 'Branca', 'Branca', 'Não-Brancos'))

## ii) Pesos ####
# Desenho amostral
desenho <- svydesign(
  ids = ~UPA,        
  strata = ~Estrato,    
  weights = ~V1028032,   
  data = base_evasao_filtrada,
  nest = TRUE           
)

## iii) Ajustar os cálculos ponderados ####
tabela_cor_raca_ponderada2 <- svytable(~V2010 + evasao + Faixa_RDPC, design = desenho)
proporcao_ponderada2 <- prop.table(tabela_cor_raca_ponderada2, margin = 1) * 100

## iv) Converter em um data.frame para visualização ####
tabela_cor_raca_ponderada_df2 <- as.data.frame(as.table(proporcao_ponderada2))
colnames(tabela_cor_raca_ponderada_df2) <- c('Cor', 'Evasao', 'RDPC', 'Proporcao')

# tabela_cor_raca_ponderada_df2 %>%
#   filter(Evasao == 1) %>%
#   filter(Cor != 'Ignorado') %>%
#   select(-Evasao) -> tabela_cor_raca_ponderada_df2

## v) Código original ####
# Resumo estatístico: Contagem e proporção de evasão por cor/raça
tabela_cor_raca2 <- tabela_cor_raca_ponderada_df2 %>%
  mutate(Proporcao = round(Proporcao, 2)) %>% 
  as.data.frame() 

# Filtrar apenas os índices de evasão
tabela_cor_raca2 <- tabela_cor_raca2 %>% 
  filter(Evasao == 1) %>% 
  filter(Cor != 'Ignorado') %>%
  arrange(desc(Proporcao)) %>% 
  select(-Evasao)

# Renomear as colunas para evitar problemas com nomes vazios ou inválidos
colnames(tabela_cor_raca2) <- c('Cor', 'RDPC', '%')

# Formatar %
tabela_cor_raca2 <- tabela_cor_raca2 %>%
  mutate(`%` = paste0(format(`%`, nsmall = 2, decimal.mark = ','))) %>% 
  arrange(Cor)

# Gerar o título dinâmico com as variáveis 'inicio' e 'fim' sem aspas
titulo_dinamico <- paste('Evasão Escolar por Cor e RDPC', paste0('Período: ', inicio, '-', fim), sep = '\n')


# Exibir a tabela com stargazer (em latex)
a_tab_resumo_cor_latex2 <- stargazer(
  tabela_cor_raca2,
  type = 'latex',
  summary = FALSE,
  title = titulo_dinamico,
  digits = 2,
  rownames = FALSE
)

## vi) Formatação final ####
# Remover as linhas de comentário geradas pelo stargazer
a_tab_resumo_cor_latex2 <- gsub('^%.*$', '', a_tab_resumo_cor_latex2)

# Adicionar uma formatação com indentação para exibição organizada
a_tab_resumo_cor_latex2 <- paste(a_tab_resumo_cor_latex2, collapse = '\n')

## vii) Exibir o resultado ####
a_tab_resumo_cor_latex2 <- paste0(
  origem,
  a_tab_resumo_cor_latex2
)
cat(a_tab_resumo_cor_latex2)


## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####
## | ####

## INFORMAÇÕES TEXTO ####


## | ####
## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 1: Público total PDM (PNADc) ####

## i) Filtrar o dataframe para o ano de 2023 ####
base_2023 <- subset(base_evasao_pdm, Ano == 2023)


## ii) Criar o design amostral ####
options(survey.lonely.psu = 'adjust')

design <- svydesign(
  ids = ~UPA,                     
  strata = ~Estrato,              
  weights = ~V1028032,           
  data = base_2023,
  nest = TRUE                    
)

## iii) Adicionar estrutura para estimativas de renda, se necessário ####
design <- convey_prep(design)

## iv) Estimar o total da população-alvo ####
populacao_pdm <- svytotal(~ensino_medio, design)

## v) Exibir os resultados ####
print(populacao_pdm)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 2: Público total PDM (PNADc) ####

## Estimar Público total PDM (PNADc) ####
# Calcular a proporção de registros de ensino médio
proporcao_alunos <- sum(base_2023$ensino_medio, na.rm = TRUE) / nrow(base_2023)

# Novo fator de ajuste considerando apenas os alunos relevantes
fator_ajuste_proporcional <- 2400000 / (sum(base_2023$V1028032, na.rm = TRUE) * proporcao_alunos)

## i) Filtrar o dataframe para o ano de 2023 ####
base_2023_2 <- subset(base_evasao_pdm, Ano == 2023)

# Criar pesos ajustados proporcionalmente
base_2023_2 <- base_2023_2 %>%
  mutate(V1028032_ajustado = ifelse(ensino_medio == 1, 
                                    V1028032 * fator_ajuste_proporcional, 
                                    0))

## ii) Criar o design amostral ####
options(survey.lonely.psu = 'adjust')

design <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~V1028032_ajustado,
  data = base_2023_2,
  nest = TRUE
)


## iii) Adicionar estrutura para estimativas de renda, se necessário ####
design <- convey_prep(design)

## iv) Estimar o total da população-alvo ####
populacao_pdm2 <- svytotal(~ensino_medio, design)

## v) Exibir os resultados ####
print(populacao_pdm2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 3: Evadidos por Região ####

## i) Pesos ####
# Criar o design amostral
pnad_design <- svydesign(
  ids = ~UPA,             # Identificação da unidade primária de amostragem
  strata = ~Estrato,      # Estratos amostrais
  weights = ~V1028032,       # Pesos amostrais calibrados
  data = base_evasao_filtrada,
  nest = TRUE             # Indica que a amostra é aninhada
)


# Filtrar evasão
evasao_ponderada <- svyby(
  ~evasao,               # Variável de interesse (1 para evasão)
  by = ~regiao,          # Agrupamento por região
  design = subset(pnad_design, evasao == 1), # Filtrar apenas evasão
  FUN = svytotal         # Função de soma ponderada
)

# Proporções ponderadas
proporcao_evasao_regiao <- svyby(
  ~evasao,               # Variável de interesse
  by = ~V1022,          # Agrupamento por região
  design = pnad_design,  # Design amostral completo
  FUN = svymean          # Média ponderada (proporção)
)



## ii) Criar df ####
proporcao_evasao_regiao %>%
  as.data.frame() %>%
  select(evasao1) %>%
  round(4) * 100 -> proporcao_evasao_regiao

colnames(proporcao_evasao_regiao) <- c('%')

## iii) Exibir o resultado ####
print(proporcao_evasao_regiao)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 4: % de brancos e não brancos por rural vs urbana que evadiram ####

# Criar a variável 'cor_raca' no data.frame original
base_evasao_filtrada$cor_raca <- ifelse(base_evasao_filtrada$V2010 == 'Branca', 'Branca',
                                        ifelse(base_evasao_filtrada$V2010 != 'Ignorado', 'Não Branca', NA))

# Remover valores NA de 'cor_raca' e 'V1022' antes de criar o svydesign
base_evasao_filtrada <- subset(base_evasao_filtrada, !is.na(cor_raca) & !is.na(V1022) & evasao == 1)

# Criar o objeto svydesign com os pesos da amostra
pnad_design <- svydesign(
  id = ~UPA, # Unidade Primária de Amostragem
  strata = ~Estrato, # Estratos da amostra
  weights = ~V1028032, # Pesos calibrados
  data = base_evasao_filtrada,
  nest = TRUE
)

# Calcular os percentuais por rural vs urbana e por cor/raça
percentuais_cor_local <- svyby(
  ~I(cor_raca == 'Branca') + I(cor_raca == 'Não Branca'),
  ~V1022,
  pnad_design,
  svymean
)

# Ajustar os resultados para exibir os percentuais em vez de proporções
percentuais_cor_local <- as.data.frame(percentuais_cor_local)

# Ajustar os percentuais com base nos nomes identificados corretamente
percentuais_cor_local$`I(cor_raca == "Branca")TRUE` <- round(percentuais_cor_local$`I(cor_raca == "Branca")TRUE` * 100, 2)
percentuais_cor_local$`I(cor_raca == "Não Branca")TRUE` <- round(percentuais_cor_local$`I(cor_raca == "Não Branca")TRUE` * 100, 2)

# Renomear colunas para maior clareza
colnames(percentuais_cor_local) <- c(
  'Area', 'Brancos', 'Não-Brancos')

# Corrigir valores da coluna 'Area'
percentuais_cor_local$Area <- ifelse(percentuais_cor_local$Area == 'Urbana Urbana', 'Urbana',
                                     ifelse(percentuais_cor_local$Area == 'Rural Rural', 'Rural', percentuais_cor_local$Area))

percentuais_cor_local %>% 
  select(Area, Brancos, `Não-Brancos`) -> percentuais_cor_local

# Ajustar os percentuais com base nos nomes identificados corretamente
percentuais_cor_local$Brancos %>%
  round(4) * 100 -> percentuais_cor_local$Brancos

# Visualizar os resultados
print(percentuais_cor_local)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 5: % de homens e mulheres que evadiram por rural vs urbana ####

# Criar a variável 'sexo' no data.frame original
base_evasao_filtrada$sexo <- ifelse(base_evasao_filtrada$V2007 == 'Homem', 'Homem',
                                    ifelse(base_evasao_filtrada$V2007 == 'Mulher', 'Mulher', NA))

# Remover valores NA de 'sexo' e 'V1022' antes de criar o svydesign
base_evasao_filtrada <- subset(base_evasao_filtrada, !is.na(sexo) & !is.na(V1022) & evasao == 1)

# Criar o objeto svydesign com os pesos da amostra
pnad_design <- svydesign(
  id = ~UPA, # Unidade Primária de Amostragem
  strata = ~Estrato, # Estratos da amostra
  weights = ~V1028032, # Pesos calibrados
  data = base_evasao_filtrada,
  nest = TRUE
)

# Calcular os percentuais por rural vs urbana e por sexo/gênero
percentuais_sexo_local <- svyby(
  ~I(sexo == 'Homem') + I(sexo == 'Mulher'),
  ~V1022,
  pnad_design,
  svymean
)

# Ajustar os resultados para exibir os percentuais em vez de proporções
percentuais_sexo_local <- as.data.frame(percentuais_sexo_local)

# Ajustar os percentuais com base nos nomes identificados corretamente
percentuais_sexo_local$`I(sexo == "Homem")TRUE` <- round(percentuais_sexo_local$`I(sexo == "Homem")TRUE` * 100, 2)
percentuais_sexo_local$`I(sexo == "Mulher")TRUE` <- round(percentuais_sexo_local$`I(sexo == "Mulher")TRUE` * 100, 2)

# Renomear colunas para maior clareza
colnames(percentuais_sexo_local) <- c(
  'Area', 'Homens', 'Mulheres')

# Corrigir valores da coluna 'Area'
percentuais_sexo_local$Area <- ifelse(percentuais_sexo_local$Area == 'Urbana Urbana', 'Urbana',
                                      ifelse(percentuais_sexo_local$Area == 'Rural Rural', 'Rural', percentuais_sexo_local$Area))

percentuais_sexo_local %>% 
  select(Homens, Mulheres) -> percentuais_sexo_local

# Ajustar os percentuais com base nos nomes identificados corretamente
percentuais_sexo_local$Homens %>%
  round(4) * 100 -> percentuais_sexo_local$Homens

# Visualizar os resultados
print(percentuais_sexo_local)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimativa 6: % de evadidos por rural vs urbana (geral) ####

base_evasao_filtrada$V1022 <- as.factor(base_evasao_filtrada$V1022)


pnad_design <- svydesign(
  id = ~UPA,              # Unidade Primária de Amostragem
  strata = ~Estrato,      # Estratos da amostra
  weights = ~V1028032,    # Pesos calibrados
  data = base_evasao_filtrada,
  nest = TRUE
)


populacao_local <- svytotal(~V1022, pnad_design, na.rm = TRUE)
print(populacao_local)

