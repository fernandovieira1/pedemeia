### GRÁFICOS E TABELAS (COM PESOS)
# - Preparação da versão final, aplicando os pesos para a população e formatando o código tex.
gc(); cat('\014')

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####
## Estimar Público total PDM (PNADc) ####

# Filtrar o dataframe para o ano de 2023
base_2023 <- subset(base_evasao_pdm, Ano == 2023)

# Criar o design amostral
options(survey.lonely.psu = "adjust")

design <- svydesign(
  ids = ~UPA,                     # Unidade Primária de Amostragem
  strata = ~Estrato,              # Estrato amostral
  weights = ~V1028032,            # Pesos amostrais (correto)
  data = base_2023,
  nest = TRUE                     # Indica estrutura aninhada
)

# Adicionar estrutura para estimativas de renda, se necessário
design <- convey_prep(design)

# Estimar o total da população-alvo
populacao_total <- svytotal(~ensino_medio, design)

# Exibir os resultados
print(populacao_total)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

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
titulo_dinamico <- paste('Proporção de Evasão por Cor/Raça', paste0('Período: ', inicio, '-', fim), sep = '\n')


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
colnames(totais_ponderados_df) <- c("Cor", "evasao", "Populacao")

## iii) Ajustar os cálculos ponderados ####
base_evasao_percentual_cor_ponderada <- prop.table(totais_ponderados, margin = 1) * 100

## iv) Converter em um data.frame para visualização
base_evasao_percentual_cor_ponderada_df <- as.data.frame(as.table(base_evasao_percentual_cor_ponderada))
colnames(base_evasao_percentual_cor_ponderada_df) <- c('Cor', 'evasao', 'Percentual')

# Combinar com o data frame de proporções
base_evasao_percentual_cor_ponderada_df <- merge(
  base_evasao_percentual_cor_ponderada_df, 
  totais_ponderados_df, 
  by = c("Cor", "evasao")
)

## v) Código original ####
# --> Comparar com as variáveis de ii) e iii)

# Calcular os percentuais de evasão por cor/raça
base_evasao_percentual_cor_ponderada_df <- base_evasao_percentual_cor_ponderada_df %>%
  mutate(
    Percentual = round(Percentual, 2),
    Populacao = format(
      round(Populacao, 0),  # Dividir por 1.000 e arredondar para duas casas decimais
      big.mark = ".",            # Ponto como separador de milhar
      decimal.mark = ",",        # Vírgula como separador decimal
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
    aes(label = paste0("(", Percentual, "%)")),  # Exibe os percentuais
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("Branca" = "lightblue", "Preta" = "#5586e3", 
               "Parda" = "#5586e3", "Amarela" = "#5586e3", 
               "Indígena" = "#5586e3")
  ) +
  labs(
    title = paste0('Evasão por Cor - Categorias Individuais - Período ', inicio, '-', fim),
    y = "Percentual (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Segundo gráfico: Branca vs Não-Brancos
grafico_agrupado <- base_evasao_percentual_cor_ponderada_df %>%
  mutate(
    Cor = ifelse(Cor == "Branca", "Branca", "Não-Brancos")  # Reclassificar as categorias
  ) %>%
  group_by(Cor, evasao) %>%
  summarise(
    Populacao = sum(as.numeric(gsub("\\.", "", Populacao))),  # Somar as populações reclassificadas
    .groups = "drop"
  ) %>%
  group_by(Cor) %>%
  summarise(
    Percentual = sum(Populacao[evasao == 1]) / sum(Populacao) * 100,  # Cálculo correto do percentual
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Cor, y = Percentual, fill = Cor)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +
  geom_text(
    aes(label = paste0("(", round(Percentual, 2), "%)")), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("Branca" = "lightblue", "Não-Brancos" = "#5586e3")
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
    legend.position = "none"
  )

# Combinar os gráficos em uma grade
grid.arrange(grafico_percentual, grafico_agrupado, ncol = 2)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.3.3A Gráfico com Percentuais no Topo ####
## i) Objeto da análise preliminar (amostral)
a_graf_sexo_percentual

## ii) Pesos ####


## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.4.1A Resumo Descritivo da Região ####
htmltools::html_print(a_tab_resumo_regiao) 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.4.3A Gráfico com Percentuais no Topo ####
a_graf_regiao_percentual 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.7.1A Resumo Descritivo do RDPC por Cor ####
htmltools::html_print(a_tab_resumo_rdpc_cor)

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

## | ####

## ++++++++++++++++++++++++++++++++ INÍCIO ++++++++++++++++++++++++++++++++ ####

## 1.12.1A Resumo Descritivo ####
htmltools::html_print(a_tab_resumo_populacao) 

## ++++++++++++++++++++++++++++++++++ FIM ++++++++++++++++++++++++++++++++ ####

